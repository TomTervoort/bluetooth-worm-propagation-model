{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, StandaloneDeriving, DeriveGeneric, DeriveAnyClass, FunctionalDependencies #-}

-- | Combines ideas from PeriodicTrigger and Broadcast models to create one simplified id model based on 
--  'signal profiles' that can each capture an address identification method, regardless of whether it is based on 
--  pasive monitoring or active inquiries.
module Model.Identification.Signal where

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
import qualified Data.Array as A

data SignalIdModel = SignalIdModel [SignalProfile] deriving (Show, Generic, FromJSON)

data SignalProfile = SignalProfile {
  senderPrevalence :: Double,
  receiverPrevalence :: Double,
  transmissionRange :: Double,
  signalFrequency :: Double
} deriving (Show, Generic, FromJSON)

type SignalProfileId = Int

instance SimulationModel IdModelInput SignalIdModel where
  data State SignalIdModel = SignalState {
    profiles  :: A.Array SignalProfileId SignalProfile,
    senders   :: [(AgentID, SignalProfileId)],
    receivers :: Set (AgentID, SignalProfileId),
    idSet     :: Set (AgentID, AgentID)
  }

  initModel agents (SignalIdModel profs) = do
    let profiles = A.listArray (0, length profs) profs
    let candidates = [(agent, profid, prof) | agent <- agents, (profid, prof) <- zip [0..] profs]
    senders <- filterAgents senderPrevalence candidates
    receivers <- S.fromList <$> filterAgents receiverPrevalence candidates
    return SignalState {idSet = S.empty, ..}
   where
    filterAgents :: (SignalProfile -> Double) -> [(AgentID, SignalProfileId, SignalProfile)] -> RNG [(AgentID, SignalProfileId)]
    filterAgents prop cands = 
      map (\(a, pid, _) -> (a, pid)) <$> filterM (\(_, _, prof) -> weightedCoinFlip $ prop prof) cands


  stepModel (IdModelInput agents infected) SignalState {..} = do
    signalers <- filterM (weightedCoinFlip . signalFrequency . (profiles A.!) . snd) senders
    let newIds = S.fromList $ concatMap signalIdentifications signalers 
    return SignalState {idSet = idSet `S.union` newIds, ..}
   where 
    infectedSet = S.fromList infected
    signalIdentifications (sender, profid) =
      let radius = transmissionRange $ profiles A.! profid
          neighbours = map snd $ KDS.inRadius (pos2Agent agents) radius $ agent2Pos agents M.! sender
       in [(receiver, sender) 
            | receiver <- neighbours, receiver `S.member` infectedSet && (receiver, profid) `S.member` receivers]


  forceState SignalState {..} x = idSet `seq` x

instance IdentificationModel SignalIdModel where
  hasIdentified a1 a2 SignalState {..} = (a1, a2) `S.member` idSet
