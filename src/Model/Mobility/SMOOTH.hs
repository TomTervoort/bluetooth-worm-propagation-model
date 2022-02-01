{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf #-}

-- | Implementation of the SMOOTH mobility model by Munjal et al. 
--   See: http://www.eurecom.fr/~spyropou/papers/Smooth-Infocom2011.pdf
--   
--   Agents are initially placed randomly. To avoid initialisation bias, the initialSkipSteps parameter (number of steps 
--   the agents will move from their random starting point before the simulation officially starts) should be set to 
--   a sufficiently high value.

module Model.Mobility.SMOOTH where

import Model.RNG
import Model.Definitions hiding (position)

import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.KdMap.Dynamic as KDM

import Control.Arrow
import Data.Ratio
import Data.Maybe
import Control.Monad
import Data.List
import Control.Monad.ST
import Data.Foldable
import System.Random
import Data.Array.ST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq, (<|), (|>), (><), Seq(..))
import qualified Data.Sequence as SE

data SMOOTHModel = SMOOTHModel {
  bounds :: (Double, Double),
  waypointCount :: Int,
  communicationRange :: Double,
  clusterCount :: Int,
  visitedWaypointFraction :: Double,
  latpExponent :: Double,
  pauseTime :: PowerLawDistribution,
  agentSpeed :: Double,
  initialSkipSteps :: Int
} deriving (Show, Generic, ToJSON)

defaultParams :: SMOOTHModel
defaultParams = SMOOTHModel {
  bounds = (1000, 1000),
  waypointCount = 700,
  clusterCount = 10,
  communicationRange = 10,
  visitedWaypointFraction = 0.1,
  latpExponent = 3,
  pauseTime = PowerLawDistribution 2 (1, 4200),
  agentSpeed = 1,
  initialSkipSteps = 200
}

withDefaultObject :: ToJSON a => a -> (Value -> Parser a) -> (Value -> Parser a)
withDefaultObject defaultVal parser val = join $ parser <$> mergeObjects val (toJSON defaultVal)
  where
    mergeObjects (Object a) (Object b) = return $ Object $ a `HM.union` b
    mergeObjects _ _ = fail "SMOOTHModel parse error: not a JSON object"

instance FromJSON SMOOTHModel where
  parseJSON = withDefaultObject defaultParams $ genericParseJSON defaultOptions

data AgentState = AgentState {
  position :: Point,
  plannedPath :: [Point],
  pauseSteps :: Int -- Zero when not pausing.
} deriving (Show)

-- Uniformly select a sequence of n positive integers that sum up to total.
-- Uses approach from https://math.stackexchange.com/a/1276225
randomSumOfLength :: Int -> Int -> RNG [Int]
randomSumOfLength n total | total < n = error "total >= n required"
                          | otherwise = map (+1) <$> randSumWithZeroes n (total - n)
  where
    randSumWithZeroes n total = compute total 0 <$> sort <$> fst <$> drawWithoutReplacement (n - 1) [1 .. total + n - 1]
    compute total prev [] = [total + n - prev - 1]
    compute total prev (x:xs) = x - prev - 1 : compute total x xs

-- Given a cluster size, select an amount of groups to divide it in. 
-- Select by proportionally preferring smaller counts.
pickGroupCount :: Int -> RNG Int
pickGroupCount n = weightedSample (\i -> n - i) [1 .. n]

instance SimulationModel () SMOOTHModel where
  data State SMOOTHModel = SMOOTHState {
    params :: SMOOTHModel,
    agents :: [(AgentID, AgentState)],
    wpClusters :: [Seq Point]
  }

  initModel agents params@(SMOOTHModel {..}) = do
    agents <- map (second (\p -> AgentState p [] 0)) <$> randomlyPlaceAgents bounds agents
    clusterSizes <- randomSumOfLength clusterCount waypointCount
    wpClusters <- mapToSeqs <$> initClusters 0 (KDM.empty pointList) clusterSizes
    multiStep initialSkipSteps SMOOTHState {..}
    where
      initClusters _ result [] = return result
      initClusters i result (csize:rest) = do
        groupCount <- pickGroupCount csize
        (g:gs) <- randomSumOfLength groupCount csize
        firstGroup@(_ :|> lastWP) <- initFirstGroup result g
        otherGroups <- initOtherGroups lastWP gs
        let result' = KDM.batchInsert result $ map (,i) $ toList $ firstGroup >< otherGroups
        initClusters (i + 1) result' rest

      initFirstGroup otherClusters n = do 
        wp <- retryUntil (null . KDM.inRadius otherClusters communicationRange) randomPoint
        finishGroup (n - 1) $ SE.singleton wp
      initOtherGroups _ [] = return SE.empty
      initOtherGroups lastWP (n:ns) = do
        headWP <- pointBetweenRadii lastWP (yConstant / 4) (yConstant / 3)
        headGroup@(_ :|> headLastWP) <- (finishGroup (n - 1) $ SE.singleton headWP :: RNG (Seq Point))
        tailGroups <- initOtherGroups headLastWP ns
        return $ headGroup >< tailGroups
      (horBound, verBound) = bounds

      finishGroup 0 group = return group
      finishGroup todo done = do
        neighbour <- (SE.index done) <$> uniformFromRange (0, SE.length done - 1)
        waypoint <- pointBetweenRadii neighbour 0 (0.1 * communicationRange)
        finishGroup (todo - 1) $ done |> waypoint

      randomPoint = Point <$> uniformFromRange (0, fst bounds) <*> uniformFromRange (0, snd bounds)
      pointBetweenRadii (Point x y) rmin rmax = retryUntil inArea $ do
        -- Based on http://www.anderswallin.net/2009/05/uniform-random-points-in-a-circle-using-polar-coordinates/
        -- And simply keeps discarding points below rmin.
        r <- retryUntil (>= rmin)  $ (rmax *) <$> sqrt <$> uniformFromRange (0, 1)
        a <- uniformFromRange (0, 2 * pi)
        return $ Point (x + r * sin a) (y + r * cos a)
      inArea (Point x y) = x >= 0 && y >= 0 && x < horBound && y < verBound

      mapToSeqs m = map (SE.fromList . map fst) $ groupBy (\(_,a) (_,b) -> a == b) $ sortOn snd $ KDM.assocs m
      yConstant = (2 * sqrt (horBound * verBound)) / fromIntegral clusterCount 

      multiStep 0 state = return state
      multiStep n state = stepModel () state >>= multiStep (n - 1)

  stepModel () (SMOOTHState {params=params@SMOOTHModel {..}, ..}) = do
    let (ids, states) = unzip agents
    agents' <- zip ids <$> forM states stepAgent
    return SMOOTHState {agents=agents', ..}
    where
      stepAgent (AgentState pos path pause) | pause > 0 = return $ AgentState pos path (pause - 1)
      stepAgent (AgentState pos@(Point x y) path 0) = do
        path'@(dpos@(Point destx desty):pathTail) <- if null path then pickPath else return path
        if distance pos dpos < agentSpeed
          then do
            pause <- powerLawRandom pauseTime
            return $ AgentState pos pathTail (floor pause)
          else do
            let phi = atan2 (desty - y) (destx - x)
            let newPos = Point (x + agentSpeed * cos phi) (y + agentSpeed * sin phi)
            return $ AgentState newPos path' 0
        where
          distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
          pickPath = do
            cluster <- toList <$> weightedSample SE.length wpClusters
            let pathLength = intFrac visitedWaypointFraction $ length cluster
            visitSet <- SE.fromList <$> fst <$> drawWithoutReplacement pathLength cluster
            latp visitSet
          latp visitSet | SE.null visitSet = return []
                        | otherwise = do
            i <- weightedSample (latpPreference . (SE.index visitSet)) [0 .. SE.length visitSet - 1]
            let wp = SE.index visitSet i
            rest <- latp $ SE.deleteAt i visitSet
            return $ wp : rest
          latpPreference wp = (1 / distance pos wp) ** latpExponent
          intFrac frac total = round (frac * fromIntegral total) `min` total `max` 1
  

instance MobilityModel SMOOTHModel where
  checkAgents SMOOTHState{..} = agentsFromList $ map (second position) agents 
