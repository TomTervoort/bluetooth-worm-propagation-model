{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf #-}

-- | Simple Boundless Simulation Area model by Haas 
--   See: https://ieeexplore.ieee.org/abstract/document/627227

module Model.Mobility.BSA where

import Model.RNG
import Model.Definitions

import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
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

import Data.Fixed (mod')

data BSAModel = BSAModel {
  bounds :: (Double, Double),
  maxVelocity :: Double,
  maxAcceleration :: Double,
  maxTurn :: Double
} deriving (Show, Generic, FromJSON)

instance SimulationModel () BSAModel where
  data State BSAModel = BSAState {
    params :: BSAModel,
    agents :: AgentPositions,
    velocityAngles :: Map AgentID (Double, Double)
  }

  initModel agents model = do
    let params = model
    let velocityAngles = M.fromList [(a, (0,0)) | a <- agents]
    agents <- agentsFromList <$> randomlyPlaceAgents (bounds model) agents
    return $ BSAState {..}
      

  stepModel () state@(BSAState {..}) = do
    velocityAngles' <- forM velocityAngles updateVA
    return $ state {
      agents = movedAgents, 
      velocityAngles = velocityAngles'
    }
    where
      movedAgents = moveAgents (\a -> setStep (velocityAngles M.! a)) agents
      (width, height) = bounds params
      setStep (velocity, angle) (Point x y) = 
        Point (wrap width $ x + cos angle * velocity) (wrap height $ y + sin angle * velocity)
      updateVA (v, a) = do
        vdelta <- uniformFromRange $ negRange $ maxAcceleration params
        adelta <- uniformFromRange $ negRange $ maxTurn params
        return (clamp (negRange $ maxVelocity params) (v + vdelta), mod' (a + adelta) (2 * pi))
      clamp (bottom, top) val = min top $ max bottom val
      negRange x = (-x,x)
      wrap maxval x | x < 0 = wrap maxval $ x + maxval
                    | x > maxval = wrap maxval $ x - maxval
                    | otherwise = x


instance MobilityModel BSAModel where
  checkAgents = agents
  