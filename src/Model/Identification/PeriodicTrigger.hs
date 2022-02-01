{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf #-}

module Model.Identification.PeriodicTrigger (PeriodicTriggerDiscoveryMethod (..)) where

import Model.RNG
import Model.Definitions
import Model.Identification.Discovery

import Control.Monad.Random
import Control.DeepSeq

import Generics.Deriving (Generic)
import Data.Random.Normal
import Data.List
import Data.Bifunctor
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

data PeriodicTriggerDiscoveryMethod = PeriodicTriggerDiscoveryMethod {
  behaviours :: [TriggerProfile]
} deriving (Show, Generic, FromJSON)
  
data TriggerProfile = TriggerProfile {
  prevalence :: Double,                -- Chance per agent that they have this behaviour
  frequencyDist :: NormalDistribution, -- Trigger chance per step is drawn from this distribution
  discoveryTimeLimit :: Maybe Int      -- Nothing is infinite.
} deriving (Show, Generic, FromJSON)


data AgentState 
  = AlwaysDiscoverableState
  | SometimesDiscoverableState {
      triggers :: [(Double, Int)], -- chance x duration
      stepsUntilHidden :: Int      -- Stays 0 as long as the agent is not currently discoverable
    } deriving (Generic, NFData)

instance SimulationModel IdModelInput PeriodicTriggerDiscoveryMethod where
  data State PeriodicTriggerDiscoveryMethod = PTState (Map AgentID AgentState)

  initModel agents (PeriodicTriggerDiscoveryMethod {..}) = do
    initStates <- forM agents (const initAgent)
    let initMap = M.fromList [(id, state) | (id, mstate) <- zip agents initStates, state <- maybeToList mstate]
    return $ PTState initMap
   where
    initAgent = do 
      profiles <- filterM (weightedCoinFlip . prevalence) behaviours >>= shuffle
      ts <- forM profiles $ \prof -> do 
        chance <- normalRandom $ frequencyDist prof
        return (chance, discoveryTimeLimit prof)
      if | null ts                  -> return Nothing
         | any (isNothing . snd) ts -> return $ Just AlwaysDiscoverableState
         | otherwise                -> do 
           let triggers = map (second fromJust) ts
           steps <- maximum <$> forM triggers (uncurry decideInitSteps)
           return $ Just $ SometimesDiscoverableState triggers steps
    
    decideInitSteps tchance tduration = do
      alreadyActivated <- weightedCoinFlip (1 - (1 - tchance) ** fromIntegral tduration)
      if alreadyActivated
       then weightedSample id [1..tduration]
       else return 0

  stepModel _ (PTState states) = PTState <$> forM states stepAgent
   where 
    stepAgent AlwaysDiscoverableState = return AlwaysDiscoverableState
    stepAgent SometimesDiscoverableState {..} = do
      stepTriggers <- map snd <$> filterM (weightedCoinFlip . fst) triggers
      let newCount = foldl1' max $ [0, stepsUntilHidden - 1] ++ stepTriggers
      return SometimesDiscoverableState {stepsUntilHidden = newCount, ..}

  forceState (PTState as) x = as `deepseq` x


instance DiscoveryModel PeriodicTriggerDiscoveryMethod where
  isDiscoverable agent (PTState states) = 
    case M.lookup agent states of
      Just AlwaysDiscoverableState -> True
      Just (SometimesDiscoverableState _ steps) -> steps /= 0
      Nothing -> False

