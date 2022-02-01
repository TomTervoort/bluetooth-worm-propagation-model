{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts #-}


-- | Helper that specifies a simplified 'discovery model' which describes whether an agent is in a discoverable state 
--   during a step.
--   When an infected agent A is close to a discoverable agent B long enough, A will identify B.
module Model.Identification.Discovery where

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
import Data.List
import Control.Monad
import Control.Exception
import Data.Foldable
import System.Random
import Data.Array.ST
import Data.Map.Strict (Map)
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S


class SimulationModel IdModelInput m => DiscoveryModel m where
  isDiscoverable :: AgentID -> State m -> Bool

data DiscoveryIdModel dm = DiscoveryIdModel {
  discoveryModel :: dm,
  transmisisonRadius :: Double,
  discoveryTime :: Int
} deriving (Generic, FromJSON, Show)

data IdStatus = Identified
              | StepsLeft Int
              deriving (Generic, NFData)

updateCollectionStatus :: DiscoveryModel dm => AgentPositions -> State (DiscoveryIdModel dm) -> State (DiscoveryIdModel dm)
updateCollectionStatus agents DiscoveryState {..} = 
  DiscoveryState {collectedIDs = M.mapWithKey updateCollector collectedIDs, ..}
  where
    DiscoveryIdModel {..} = params
    neighbours agent = map snd $ KDS.inRadius (pos2Agent agents) transmisisonRadius $ agent2Pos agents M.! agent
    discoverableNeighbours agent = filter (\a -> isDiscoverable a dstate) $ neighbours agent
    keepIdentified Identified = Just Identified
    keepIdentified (StepsLeft _) = Nothing
    idProgress Identified = Identified
    idProgress (StepsLeft 1) = Identified
    idProgress (StepsLeft n) = StepsLeft $ n - 1
    initialStepsLeft | discoveryTime == 0 = Identified
                     | otherwise = StepsLeft discoveryTime
    updateCollector collector memory = 
      merge (mapMaybeMissing (\_ s -> keepIdentified s))  -- In memory but no neighbour
            (mapMissing (\_ () -> initialStepsLeft))      -- Neighbour but not yet in memory
            (zipWithMatched (\_ s () -> idProgress s))    -- Both in memory and a neighbour
            memory 
            (M.fromList $ zip (discoverableNeighbours collector) $ repeat ())


instance DiscoveryModel dm => SimulationModel IdModelInput (DiscoveryIdModel dm) where
  data State (DiscoveryIdModel dm) = DiscoveryState {
    params :: DiscoveryIdModel dm,
    dstate :: State dm,
    collectedIDs :: Map AgentID (Map AgentID IdStatus) -- Collector -> (target -> status)
  }

  initModel agents params@(DiscoveryIdModel {..}) = do
    dstate <- initModel agents discoveryModel
    let collectedIDs = M.empty
    return DiscoveryState {..}

  stepModel inp@(IdModelInput agents infected) (DiscoveryState {..}) = updateCollectionStatus agents <$> do
    dstate' <- stepModel inp dstate
    let collectedIDs' = M.unionWith M.union collectedIDs $ M.fromList [(a, M.empty) | a <- infected]
    return $ DiscoveryState params dstate' collectedIDs'

  forceState (DiscoveryState _ ds ids) x = forceState ds `seq` collectedIDs `deepseq` x

instance DiscoveryModel dm => IdentificationModel (DiscoveryIdModel dm) where
  hasIdentified a b (DiscoveryState {..}) = 
    case M.lookup b $ M.findWithDefault M.empty a collectedIDs of
      Just Identified -> True
      _ -> False
  currentlyExposed agent (DiscoveryState {..}) = isDiscoverable agent dstate

