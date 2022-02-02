{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, ScopedTypeVariables, 
             FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- | Collection of basic data types, type class definitions and some utility functions.
--   The SimulationModel type class represents any type of non-deterministic step-by-step simulation model. Its 
--   parameters represent the step inputs. Subclassess can add functions that observe the model state, which is used
--   to represent intermediate outputs.
--   The PropagationModel type is a SimulationModel that requires no inputs. It combines three instances of component 
--   models into a full Bluetooth propagation model.
module Model.Definitions where

import Model.RNG

import Control.Parallel
import Control.DeepSeq
import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Aeson
import Data.KdMap.Static (KdMap)
import qualified Data.KdMap.Static as KDS

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.List
import System.Random
import Data.Array.ST
import Control.Exception
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

-- Useful data types.

newtype AgentID = AgentID Int deriving (Eq, Ord, Show, Generic, NFData)

data AgentReport = AgentReport {
  position :: Point,
  infected :: Bool,
  identifiable :: Bool,
  identified :: Bool,
  immune :: Bool,
  currentlyTargeting :: [Point]
} deriving (Show, Generic, ToJSON)


-- Store agent positions using a bimap that uses a 2-d tree for the point -> agent mapping.
data Point = Point {x :: Double, y :: Double} deriving (Eq, Ord, Generic, Show, ToJSON, NFData)
data AgentPositions = AgentPositions {
  pos2Agent :: KdMap Double Point AgentID,
  agent2Pos :: Map AgentID Point
}

pointList :: KDS.PointAsListFn Double Point
pointList (Point x y) = [x, y]

-- Sub-model interfaces

class (FromJSON m, Show m) => SimulationModel stepinput m | m -> stepinput where
  data State m
  initModel :: [AgentID] -> m -> RNG (State m)
  stepModel :: stepinput -> State m -> RNG (State m)

  -- Can be used to force implementation-defined evaluation of state to allow parallelism. 
  -- Only evaluates to WHNF by default.
  forceState :: State m -> b -> b
  forceState = pseq

class SimulationModel () m => MobilityModel m where
  checkAgents      :: State m -> AgentPositions

data IdModelInput = IdModelInput {positionsForId :: AgentPositions, infectedForId :: [AgentID]}
class SimulationModel IdModelInput m => IdentificationModel m where
  hasIdentified     :: AgentID -> AgentID -> State m -> Bool -- True when the first agent has identified the second since it started ID collection.
  
  -- Below methods are optional, for visualization only.

  currentlyExposed :: AgentID -> State m -> Bool
  -- What exposure means depends on the model
  currentlyExposed _ _ = False

data InfModelInput = InfModelInput {positionsForInf :: AgentPositions, agentHasId :: AgentID -> AgentID -> Bool}
class SimulationModel InfModelInput m => InfectionModel m where
  infectedAgents :: State m -> [AgentID] -- All currently infected agents
  
  -- Below methods are optional, for visualization only.

  isSusceptible :: AgentID -> State m -> Bool
  -- Whether a certain agent is susceptible to infection. Result undefined for already infected agents.
  isSusceptible _ _ = True

  currentAttackTargets :: AgentID -> State m -> [AgentID]
  -- Given an attacker and a state, the result are other agent this attacker is currently focusing its attack on. 
  -- What this means is model-specific.
  currentAttackTargets _ _ = []


-- Composition into propagation model.
data PropagationModel mob id inf = PropagationModel {
  mobModel :: mob,
  idModel  :: id,
  infModel :: inf
} deriving (Show, Generic, FromJSON)

instance (MobilityModel mob, IdentificationModel id, InfectionModel inf) 
  => SimulationModel () (PropagationModel mob id inf) where
    data State (PropagationModel mob id inf) = PropState {
      mobState :: State mob,
      idState  :: State id,
      infState :: State inf,
      -- Precomputed (lazy and in parallel) mob state and corresponding agents of the next step.
      nextMobState       :: State mob,
      nextMobStateAgents :: AgentPositions
    }

    initModel agents (PropagationModel mob id inf) = do
      -- Split RNG during initialization so the same seed will initiate each sub-model in the same manner, even if 
      -- parameters of the other sub-models change. This is useful for testing. Of course do make sure to not reuse 
      -- seeds when running multiple experiments.
      mobState <- interleave $ initModel agents mob
      idState  <- interleave $ initModel agents id
      infState <- interleave $ initModel agents inf
      nextMobState <- stepModel () mobState
      let nextMobStateAgents = checkAgents nextMobState
      return PropState {..}

    stepModel () (PropState _ ids infs mobs' as') = do
      (mobs'', as'') <- parRNG forceMob $ stepMob mobs'
      ids' <- stepModel (IdModelInput as' (infectedAgents infs)) ids
      infs' <- stepModel (InfModelInput as' (\a1 a2 -> hasIdentified a1 a2 ids')) infs
      return $ PropState mobs' ids' infs' mobs'' as''
      where
        stepMob st = do
          st' <- stepModel () st
          return (st', checkAgents st')
        forceMob (_, as) = pos2Agent as `deepseq` ()

    forceState (PropState mob id inf _ _) x = forceState mob $ forceState id $ forceState inf x

agentReport :: (MobilityModel mob, IdentificationModel id, InfectionModel inf) => 
               AgentID -> State (PropagationModel mob id inf) -> AgentReport
agentReport agent (PropState mobs ids infs _ _) = AgentReport {
  position = agent2Pos agentPositions M.! agent,
  infected = isInfected,
  identifiable = currentlyExposed agent ids,
  identified = any (\attacker -> hasIdentified attacker agent ids) $ infectedAgents infs,
  immune = not (isInfected || isSusceptible agent infs),
  currentlyTargeting = if isInfected then map (agent2Pos agentPositions M.!) $ currentAttackTargets agent infs else []
 }
  where 
    agentPositions = checkAgents mobs
    isInfected = agent `elem` infectedAgents infs

-- | Parallel model composition (same input). When used with an IdentificationModel identified sets are joined.
data ParallelModels m1 m2 = ParallelModels m1 m2 deriving (Show, Generic)
type ParallelModels3 m1 m2 m3 = ParallelModels m1 (ParallelModels m2 m3)


instance (SimulationModel inp m1, SimulationModel inp m2) => SimulationModel inp (ParallelModels m1 m2) where
  data State (ParallelModels m1 m2) = ParState (State m1) (State m2)

  initModel agents (ParallelModels a b) = ParState <$> initModel agents a <*> initModel agents b
  stepModel inp (ParState a b) =  ParState <$> parStep inp a <*> parStep inp b
    where
      parStep inp = parRNG (flip forceState ()) . stepModel inp

  forceState (ParState a b) x = forceState a $ forceState b x

instance (IdentificationModel m1, IdentificationModel m2) => IdentificationModel (ParallelModels m1 m2) where
  hasIdentified a1 a2 (ParState s1 s2) = hasIdentified a1 a2 s1 || hasIdentified a1 a2 s2
  currentlyExposed a (ParState s1 s2) = currentlyExposed a s1 || currentlyExposed a s2

instance (FromJSON a, FromJSON b) => FromJSON (ParallelModels a b) where
  parseJSON val = uncurry ParallelModels <$> parseJSON val

-- Agent helpers.

moveAgents :: (AgentID -> Point -> Point) -> AgentPositions -> AgentPositions
moveAgents updater ps = agentsFromList [(id, updater id p)  | (id, p) <- M.assocs $ agent2Pos ps]

-- Uniformly place agents on a two-dimensial grid.
randomlyPlaceAgents :: (Double, Double) -> [AgentID] -> RNG [(AgentID, Point)]
randomlyPlaceAgents (width, height) agents = zip agents <$> forM agents (const randomPoint)
  where
    randomPoint = Point <$> uniformFromRange (0, width) <*> uniformFromRange (0, height)

agentsFromList :: [(AgentID, Point)] -> AgentPositions
agentsFromList entries = AgentPositions {..}
  where
    pos2Agent = KDS.build pointList [(pos, agent) | (agent, pos) <- entries]
    agent2Pos = M.fromList entries

