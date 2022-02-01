{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf #-}

module Model.Infection.Simple where

import Model.RNG
import Model.Definitions

import Control.Monad.Random

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


data SimpleInfectionModel = SimpleInfectionModel {
  initialInfectionCount :: Int,
  infectionDistance     :: Double,
  infectionTime         :: Integer,
  failChance            :: Double
} deriving (Show, Generic, FromJSON)

instance SimulationModel InfModelInput SimpleInfectionModel where
  data State SimpleInfectionModel = SimState {
    distParam          :: Double,
    timeParam          :: Integer,
    failChanceParam    :: Double,
    infectionProgress  :: Map AgentID (Map AgentID Integer) -- infected => (target => continious steps)
  }

  initModel agents (SimpleInfectionModel {..}) = do
    let (distParam, timeParam, failChanceParam) = (infectionDistance, infectionTime, failChance)
    infectedAgents <- fst <$> drawWithoutReplacement initialInfectionCount agents
    let infectionProgress = M.fromList $ map (,M.empty) infectedAgents
    return SimState {..}

  stepModel (InfModelInput positions agentHasId) SimState {..} = do
    infectionProgress <- processAttacks positions infectionProgress $ M.keys infectionProgress
    return SimState {..}

    where
      processAttacks _ progress [] = return progress
      processAttacks positions progress (attacker:rest) = do
        let susceptible target = M.notMember target infectionProgress && agentHasId attacker target
        let inRange = map snd $ KDS.inRadius (pos2Agent positions) distParam $ agent2Pos positions M.! attacker
        let victims = filter susceptible inRange
        (newAttackers, victimProgress) <- partitionVictims (progress M.! attacker) victims 
        let progress' = M.union (M.insert attacker victimProgress progress) (M.fromList $ map (, M.empty) newAttackers)
        processAttacks positions progress' rest

      partitionVictims _ [] = return ([], M.empty)
      partitionVictims prevProg (v:vs) = do
        (newAttackers, victimProgress) <- partitionVictims prevProg vs
        let counter = M.findWithDefault 0 v prevProg + 1

        if counter < timeParam
          then return (newAttackers, M.insert v counter victimProgress)
          else do
            failed <- weightedCoinFlip failChanceParam
            if failed 
              then return (newAttackers, M.insert v 0 victimProgress)
              else return (v : newAttackers, victimProgress)
            

instance InfectionModel SimpleInfectionModel where
  infectedAgents (SimState {..}) = M.keys infectionProgress

