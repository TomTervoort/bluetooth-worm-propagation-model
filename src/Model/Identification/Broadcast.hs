{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, StandaloneDeriving, DeriveGeneric, DeriveAnyClass, FunctionalDependencies #-}

-- | Model where some agents periodically 'broadcast' their identity to a subset of collectors capable of receiving this 
--   broadcast. Represents MAC address correlation by Wi-Fi snoopers.
--   Currently uses a normal distribution to hand out broadcast frequencies.
module Model.Identification.Broadcast where

import Model.RNG
import Model.Definitions

import Control.Monad.Random
import Control.DeepSeq

import Generics.Deriving (Generic)
import Control.Monad.Random
import Data.Aeson
import Data.KdMap.Static (KdMap)
import qualified Data.KdMap.Static as KDS
import Data.Foldable

import Data.Maybe
import Control.Monad
import Control.Exception
import System.Random
import Data.Array.ST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S


-- | Allows composition with another identification model. 
data BroadcastModel = BroadcastModel {
  broadcasterCount :: Int,
  receiverCount :: Int,
  frequencyDistribution :: NormalDistribution,
  broadcastRadius :: Double,
  identificationChance :: Double
} deriving (Show, Generic, FromJSON)

instance SimulationModel IdModelInput BroadcastModel where
  data State BroadcastModel = BState {
    radius :: Double,
    idChance :: Double,
    broadcasters :: Map AgentID Double, -- Value is broadcast chance per step.
    potentialReceivers :: Set AgentID,
    idsCollected :: Set (AgentID, AgentID)
  } deriving (Generic, NFData)

  initModel agents BroadcastModel {..} = do
    let radius = broadcastRadius
    let idChance = identificationChance
    broadcastAgents <- fst <$> drawWithoutReplacement broadcasterCount agents
    broadcasters <- (M.fromList <$>) $ forM broadcastAgents $ \agent -> do
      freq <- normalRandom frequencyDistribution
      return (agent, freq)
    let broadcastingThisStep = S.empty
    potentialReceivers <- S.fromList <$> fst <$> drawWithoutReplacement receiverCount agents
    let idsCollected = S.empty
    return BState {..}

  stepModel (IdModelInput agents infected) BState {..} = do
    let currentReceivers = S.fromList infected `S.intersection` potentialReceivers
    newIds <- forM (M.assocs broadcasters) $ \(broadcaster, chance) -> do
      shouldBroadcast <- weightedCoinFlip chance
      if not shouldBroadcast then return S.empty else do
        let neighbours = map snd $ KDS.inRadius (pos2Agent agents) radius $ agent2Pos agents M.! broadcaster
        identifyingNeighbours <- filterM (\_ -> weightedCoinFlip idChance) neighbours
        return $ S.map (, broadcaster) $ S.fromList identifyingNeighbours `S.intersection` currentReceivers
    return BState {idsCollected = foldr S.union idsCollected newIds, ..}

  forceState = deepseq

instance IdentificationModel BroadcastModel where
  hasIdentified a1 a2 BState {..} = (a1, a2) `S.member` idsCollected
  currentlyExposed a BState {..} = False
