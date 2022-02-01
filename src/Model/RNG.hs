{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, ScopedTypeVariables, FunctionalDependencies #-}

-- | Random number monad and helpers.
module Model.RNG where

import Control.Parallel.Strategies (withStrategy, rpar)

import Control.Parallel
import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Aeson
import Data.KdMap.Static (KdMap)
import qualified Data.KdMap.Static as KDS
import Data.Number.Erf (inverf)

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.List
import System.Random
import Data.Array.ST
import Control.Exception
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Fail

-- RNG monad.
newtype RNG a = RNG (Rand StdGen a) deriving (Applicative, Monad, MonadRandom)

instance Functor RNG where
  fmap f (RNG m) = RNG $ fmap f m

instance MonadFail RNG where
  fail = error

instance MonadSplit StdGen RNG where
  getSplit = RNG getSplit

instance MonadInterleave RNG where
  interleave (RNG op) = RNG (interleave op)

-- Probability distributions used for modelling.

data NormalDistribution = NormalDistribution {
  mean   :: Double,
  stdDev :: Double
} deriving (Show, Generic)

data PowerLawDistribution = PowerLawDistribution {
  alpha :: Double,
  distRange :: (Double, Double)
} deriving (Show, Generic)

data WeibullDistribution = WeibullDistribution {
  scale :: Double,
  shape :: Double
} deriving (Show, Generic)

data LevyDistribution = LevyDistribution {
  coefficient :: Double,
  levyScale :: Double
} deriving (Show, Generic)

instance FromJSON NormalDistribution where
instance FromJSON PowerLawDistribution where
instance FromJSON WeibullDistribution where
instance FromJSON LevyDistribution where
instance ToJSON NormalDistribution where
instance ToJSON PowerLawDistribution where
instance ToJSON WeibullDistribution where
instance ToJSON LevyDistribution where

-- Inclusive.
uniformFromRange :: UniformRange a => (a, a) -> RNG a
uniformFromRange range = RNG $ liftRand (uniformR range)

shuffle :: [a] -> RNG [a]
shuffle xs = fst <$> drawWithoutReplacement (length xs) xs
  
-- Extracts count elements from xs in a random order, in O(length xs). Second result element is the remaining elements 
-- in an undefined order.
drawWithoutReplacement :: forall a. Int -> [a] -> RNG ([a],[a])
drawWithoutReplacement count xs = RNG $ liftRand $ \g -> runST (drawST g)
  where
    xsSize = length xs
    drawST :: StdGen -> ST s (([a], [a]), StdGen)
    drawST g = do
      arr <- newListArray (0, xsSize - 1) xs
      g' <- doSwaps g 0 arr
      shuffled <- getElems arr
      return (splitAt count shuffled, g')
    doSwaps :: StdGen -> Int -> STArray s Int a -> ST s StdGen
    doSwaps g i1 arr | i1 >= count = return g
                     | otherwise = do 
      let (i2, g') = uniformR (i1, xsSize - 1) g
      e1 <- readArray arr i1
      e2 <- readArray arr i2
      writeArray arr i1 e2
      writeArray arr i2 e1
      doSwaps g' (i1 + 1) arr

randomElement :: [a] -> RNG a
randomElement = weightedSample (const (1 :: Int))

-- Argument is chance of True.
weightedCoinFlip :: Double -> RNG Bool
weightedCoinFlip p = (< p) <$> uniformFromRange (0, 1)

weightedSample :: (Num n, Ord n, UniformRange n) => (a -> n) -> [a] -> RNG a
weigtedSample _ [] = error "Empty list"
weightedSample weight elems = do
  let accumWeights = scanl1 (+) $ map weight elems
  r <- uniformFromRange (0, last accumWeights)
  return $ maybe (last elems) (elems !!) $ findIndex (> r) accumWeights

normalRandom :: NormalDistribution -> RNG Double
normalRandom (NormalDistribution m d) = RNG $ liftRand $ normal' (m, d)

-- Based on https://mathworld.wolfram.com/RandomNumber.html
powerLawRandom :: PowerLawDistribution -> RNG Double
powerLawRandom (PowerLawDistribution alpha (x0, x1)) = assert inputValid $ convert <$> uniformFromRange (0, 1)
  where
    inputValid = x0 > 0 && alpha > 0 && x0 < x1
    exponent = -alpha + 1
    convert y = ((x1 ** exponent - x0 ** exponent) * y + x0 ** exponent) ** (1 / exponent)

-- Based on https://www.taygeta.com/random/weibull.html
weibullRandom :: WeibullDistribution -> RNG Double
weibullRandom (WeibullDistribution a b) = convert <$> uniformFromRange (0, 1)
  where 
    convert y = ((-1 / a) * log (1 - y)) ** (1 / b)

-- Based on https://en.wikipedia.org/wiki/Lévy_distribution#Random_sample_generation
-- and https://stackoverflow.com/questions/64825826/write-a-random-number-generator-that-based-on-uniformly-distributed-numbers-bet
levyRandom :: LevyDistribution -> RNG Double
levyRandom (LevyDistribution mu c) = convert <$> uniformFromRange (0, 1)
  where
    convert u = mu + c / (probit (1.0 - u))^2
    probit p = sqrt2 * inverf (2 * p - 1)
    sqrt2 = sqrt 2

retryUntil :: (a -> Bool) -> RNG a -> RNG a
retryUntil pred rng = do
  x <- rng
  if pred x then return x else retryUntil pred rng


-- | Will initialize parallel  evaluatation of (force op) before returning its lazy result.
parRNG :: (a -> ()) -> RNG a -> RNG a
parRNG force op = withStrategy rpar . fseq . fst . runRNG op <$> getSplit
  where 
    fseq x = force x `pseq` x

runRNG :: RNG a -> StdGen -> (a, StdGen)
runRNG (RNG m) g = runRand m g
