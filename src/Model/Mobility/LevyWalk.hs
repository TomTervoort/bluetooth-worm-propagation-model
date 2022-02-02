{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf, OverloadedStrings #-}

-- | Levy walk mobility model by Rhee et al.
--   See: https://ieeexplore.ieee.org/document/5750071

module Model.Mobility.LevyWalk where

import Model.RNG
import Model.Definitions hiding (AgentReport(..))

import Control.DeepSeq
import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Bifunctor
import Data.Aeson
import Data.KdMap.Dynamic (KdMap)
import qualified Data.KdMap.Dynamic as KDM

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import System.Random
import Data.Array.ST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM

-- Test: state <- evalRandIO $ initModel (map AgentID [1..3]) (LevyWalkModel (100,100) 0.86 0.99 10 1 50 1000)

data LevyWalkModel = LevyWalkModel {
  bounds :: (Double, Double),
  flightLengthCoefficient :: Double,
  pauseTimeCoefficient :: Double,
  flightLengthScale :: Double,
  pauseTimeScale :: Double,
  maxPause :: Int
} deriving (Show, Generic)

-- | Only bounds and coefficients are non-optional.
instance FromJSON LevyWalkModel where
  parseJSON (Object obj) = genericParseJSON defaultOptions $ Object $ HM.union obj defaultValues
   where
    defaultValues = HM.map Number $ HM.fromList [
      ("flightLengthScale", 10),
      ("pauseTimeScale", 1)
     ]

data AgentState 
  = WalkingState {
    position :: Point, 
    hspeed :: Double, 
    vspeed :: Double, 
    stepsLeft :: Int,
    nextPauseTime :: Int
  }
  | PauseState {
    position :: Point,
    stepsLeft :: Int
  } deriving (Generic, NFData)

instance SimulationModel () LevyWalkModel where
  data State LevyWalkModel = LevyWalkState {
    params :: LevyWalkModel,
    agents :: [(AgentID, AgentState)]
  }

  initModel agentIds params = do
    agents <- map (second (\pos -> PauseState pos 0)) <$> randomlyPlaceAgents (bounds params) agentIds
    return LevyWalkState {..}

  stepModel () (LevyWalkState params@(LevyWalkModel {..}) agents) = do
    agents' <- forM agents $ \(aid, as) -> (aid, ) <$> stepAgent as
    return $ LevyWalkState params agents'
    where
      (areaw, areah) = bounds
      stepAgent (WalkingState {..}) 
        | stepsLeft == 0 = return $ PauseState position nextPauseTime
        | otherwise = 
          let Point x y = position 
           in return WalkingState {position = Point (x + hspeed) (y + vspeed), stepsLeft = stepsLeft - 1, ..}
      stepAgent (PauseState pos 0) = pickWalk pos >>= stepAgent
      stepAgent (PauseState pos n) = return $ PauseState pos (n - 1)
      levyParam coeff maxval scale =
        retryUntil (\x -> 0 <= x && x <= maxval) $ levyRandom (LevyDistribution coeff scale)
      pickWalk position = retryIfOOB $ do 
        flightLength <- levyParam flightLengthCoefficient (read "Infinity") flightLengthScale
        nextPauseTime <- round <$> levyParam pauseTimeCoefficient (fromIntegral maxPause) pauseTimeScale
        angle <- uniformFromRange (0, 2 * pi)
        let (fk, fr) | flightLength < areaw / 2 = (18.72, 0.79)
                     | otherwise = (1.37, 0.36)
        let stepsLeft = round (fk * flightLength**(1 - fr) :: Double)
        let velocity = flightLength / fromIntegral stepsLeft
        let hspeed = cos angle * velocity
        let vspeed = sin angle * velocity
        return WalkingState {..}

      retryIfOOB walkPicker = do
        walk@(WalkingState {..}) <- walkPicker
        let Point startx starty = position
        let time = fromIntegral stepsLeft
        let (destx, desty) = (startx + hspeed * time, starty + vspeed * time)
        if destx <= 0 || destx >= areaw || desty <= 0 || desty >= areah
          then retryIfOOB walkPicker
          else return walk

  forceState (LevyWalkState _ agents) x = agents `deepseq` x

instance MobilityModel LevyWalkModel where
  checkAgents LevyWalkState{..} = agentsFromList $ map (second position) agents 

