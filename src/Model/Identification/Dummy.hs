{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, FunctionalDependencies, FlexibleContexts, MultiWayIf #-}

-- | Model that considers every agent to have identified every other immediately.
module Model.Identification.Dummy where

import Model.RNG
import Model.Definitions

import Control.Monad.Random

import Generics.Deriving (Generic)
import Data.Random.Normal
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


data DummyIdModel = DummyIdModel deriving (Show, Generic)

instance FromJSON DummyIdModel where
  parseJSON Null = return DummyIdModel
  parseJSON _ = fail "Dummy parameters not null."

instance SimulationModel IdModelInput DummyIdModel where
  data State DummyIdModel = DummyState
  initModel _ _ = return DummyState
  stepModel _ _ = return DummyState

instance IdentificationModel DummyIdModel where
  hasIdentified _ _ _ = True
  currentlyExposed _ _ = True

