{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf #-}

-- | Serial infection model employed by Zyba et al.
--   See: https://ieeexplore.ieee.org/abstract/document/5062067

module Model.Infection.Serial where

import Model.RNG
import Model.Definitions

import Control.Monad.Random
import Control.DeepSeq

import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Aeson
import Data.KdMap.Static (KdMap)
import qualified Data.KdMap.Static as KDS

import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import System.Random
import Data.Array.ST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.Writer.Lazy


data SerialInfectionModel = SerialInfectionModel {
  initialInfectionCount :: Int,
  susceptibilityRate    :: Double,
  infectionDistance     :: Double,
  infectionTime         :: Int
} deriving (Show, Generic, FromJSON)

data InfectedAgentState 
  = Idle { attackedSoFar :: Set AgentID }
  | Attacking { attackedSoFar :: Set AgentID, target :: AgentID, stepsLeft :: Int }
  deriving (Generic, NFData)

instance SimulationModel InfModelInput SerialInfectionModel where
  data State SerialInfectionModel = SerState {
    params :: SerialInfectionModel,
    susceptibleAgents  :: Set AgentID, -- excludes infected agents
    infectedAgentState :: Map AgentID InfectedAgentState
  }

  initModel agents params@(SerialInfectionModel {..}) = do
    infectedAgentList <- fst <$> drawWithoutReplacement initialInfectionCount agents
    let infectedAgentState = M.fromList $ map (, Idle S.empty) infectedAgentList
    susceptibleAgents <- S.fromList <$> filterM (checkSusceptible infectedAgentState) agents
    return SerState {..}
    where
      checkSusceptible infected agent | agent `M.member` infected = return False
                                      | otherwise = weightedCoinFlip susceptibilityRate

  stepModel (InfModelInput positions agentHasId) SerState {..} = do
    (updated, newInfected)  <- runWriterT $ M.traverseWithKey updateInfectedState infectedAgentState
    return SerState {
      susceptibleAgents = susceptibleAgents S.\\ newInfected,
      infectedAgentState = M.union updated $ M.fromSet (const $ Idle S.empty) newInfected,
      ..
    }

    where
      SerialInfectionModel {..} = params
      distanceSquared (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2 + (y1 - y2)^2)
      stillInRange attacker target = 
        distanceSquared (agent2Pos positions M.! attacker) (agent2Pos positions M.! target) <= infectionDistance * infectionDistance
      
      updateInfectedState attacker Attacking {..} | stillInRange attacker target =
        if stepsLeft == 0
          then do
            when (target `S.member` susceptibleAgents) $ tell (S.singleton target)
            return Idle {attackedSoFar = S.insert target attackedSoFar}
        else
          return Attacking {stepsLeft = stepsLeft - 1, ..}
        
      updateInfectedState attacker state = do
        -- Pick a random new victim in range.
        let inRange = map snd $ KDS.inRadius (pos2Agent positions) infectionDistance 
                              $ agent2Pos positions M.! attacker
        let possibleVictims = filter (permittedTarget (attackedSoFar state) attacker) inRange
        if null possibleVictims
          then return $ Idle (attackedSoFar state)
          else do
            victim <- lift $ randomElement possibleVictims
            return $ Attacking (attackedSoFar state) victim infectionTime

      permittedTarget alreadyAttacked attacker target = 
        target /= attacker && agentHasId attacker target && target `S.notMember` alreadyAttacked

  forceState (SerState _ sus inf) x = sus `deepseq` inf `deepseq` x
            

instance InfectionModel SerialInfectionModel where
  infectedAgents (SerState {..}) = M.keys infectedAgentState
  isSusceptible agent (SerState {..}) = agent `S.member` susceptibleAgents
  currentAttackTargets attacker (SerState {..}) = 
    case M.lookup attacker infectedAgentState of
      Just (Attacking {..}) -> [target]
      _ -> []
