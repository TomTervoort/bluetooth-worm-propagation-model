{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric #-}

-- | Experimental identifcation model.

module Model.Identification.Gaussian where

import Model.Definitions

import Control.Monad.Random

import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Aeson
import Data.KdMap.Dynamic (KdMap)
import qualified Data.KdMap.Dynamic as KDM

import Data.Maybe
import Control.Monad
import Control.Exception
import Data.Foldable
import System.Random
import Data.Array.ST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S


data GaussDiscovery = GaussDiscovery {
  stateSwitchInterval :: (Int, Int),
  agentTypeCounts     :: [(AgentType, Int)],
  idDistribution      :: NormalDistribution
  -- freqIdDistribution  :: NormalDistribution,
  -- rareIdDistribution  :: NormalDistribution
} deriving (Show, Generic, FromJSON)

data AgentType = NeverIdentifiable | SometimesIdentifiable | AlwaysIdentifiable deriving (Eq, Show, Generic, FromJSON)

data AgentIdState
  = AlwaysIdState
  | SometimesIdState {activeFraction :: Double, activeNow :: Bool}
  deriving (Eq, Show)
  -- | FreqIdState {activeNow :: Bool}
  -- | RareIdState {activeNow :: Bool}


instance SimulationModel GaussDiscovery where
  data State GaussDiscovery = GaussState {
    params       :: GaussDiscovery,
    currentStep  :: Int,
    agentStates  :: Map AgentID AgentIdState,   -- Agents not in this map are not identifiable at all
    updateQueue  :: Map Int [AgentID]           -- Priority queue of step number to agents that need to be updated
  }

  initModel agents params@(GaussDiscovery {..}) = do
    assert (sum (map snd agentTypeCounts) == length agents) $ return ()
    let currentStep = 0
    agentTypes <- shuffle $ concat [replicate count kind | (kind, count) <- agentTypeCounts]
    agentStatesList <- mapMaybe (\(a, ms) -> (a,) <$> ms) . zip agents <$> forM agentTypes initAgentState
    let agentStates = M.fromList agentStatesList
    updateQueue <- makeMultimap . catMaybes <$> forM agentStatesList getSwitchTime
    return GaussState {..}

    where
      initAgentState NeverIdentifiable = return Nothing
      initAgentState AlwaysIdentifiable = return $ Just AlwaysIdState
      initAgentState SometimesIdentifiable = do
        activeFraction <- clampChance <$> normalRandom idDistribution
        activeNow <- weightedCoinFlip activeFraction
        return $ Just SometimesIdState {..}

      clampChance p | p <= 0 = 0
                    | p >= 1 = 1
                    | otherwise = p
      getSwitchTime (a, AlwaysIdState) = return Nothing
      getSwitchTime (a, SometimesIdState _ _) = Just . (,a) <$> uniformFromRange stateSwitchInterval
      makeMultimap = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty

  stepModel state@(GaussState {..}) = 
    case M.minViewWithKey updateQueue of
      Just ((step, agents), updateQueue2) | step <= currentStep -> do
        (newStates, updateQueue3) <- foldM updater (agentStates, updateQueue2) agents
        return state {
          currentStep = currentStep + 1,
          agentStates = newStates,
          updateQueue = updateQueue3
        }
      _ -> return state {currentStep = currentStep + 1}

      where
        updater (states, queue) agent = do
          let SometimesIdState chance _ = agentStates M.! agent
          shouldActivate <- weightedCoinFlip chance
          switchTime <- uniformFromRange $ stateSwitchInterval params
          return (
            M.insert agent (SometimesIdState chance shouldActivate) states, 
            M.insertWith (++) (currentStep + switchTime) [agent] queue
           )


instance DiscoveryModel GaussDiscovery where
  isDiscoverable agent state = 
    case M.lookup agent $ agentStates state of
      Just AlwaysIdState               -> True
      Just (SometimesIdState _ active) -> active
      Nothing                          -> False
