{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, ExistentialQuantification, ScopedTypeVariables, OverloadedStrings #-}

-- | Command line tool that runs (resumable) simulation experiment based on parameters in a JSON config file.

module Model.UI.ExperimentRunner where

import Model.Definitions
import Model.UI.JSON

import Data.Aeson

import Generics.Deriving (Generic)
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.State.Strict
import System.IO
import System.Exit
import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import System.Random
import Numeric

data Configuration = Configuration {
  simulation :: SimulationParams,
  
  runTime :: Int,
  experimentCount :: Int,
  outputInterval :: Int,
  outputFile :: FilePath

  -- threads :: Int,
  -- checkpointInterval :: Int,
  -- restoreFile :: FilePath
} deriving (Show, Generic, FromJSON)


csvHeaders :: String
csvHeaders = "step, mean infection rate, std. dev"

reportsToCSVLine :: Int -> [SimulationReport] -> String
reportsToCSVLine step reports = show step ++ "," ++ showFrac meanRate ++ "," ++ showFrac stdDev
 where
  showFrac x = showFFloatAlt (Just 4) x ""
  meanRate = mean rates
  stdDev = sqrt $ mean [(x - meanRate)^2 | x <- rates]
  mean xs = sum xs / dlen xs
  rates = [dlen (filter infected $ agents report) / dlen (filter (not . immune) $ agents report) | report <- reports]
  dlen x = fromIntegral $ length x :: Double
  

-- Invokes callback with a write-only handle that writes to a file, or to stdout if the path is "-".
writeToFileOrStdout :: FilePath -> (Handle -> IO a) -> IO a
writeToFileOrStdout "-" op = op stdout
writeToFileOrStdout path op = withFile path WriteMode op

expandSeed :: Int -> Int -> [Int]
expandSeed count seed = (take count . randoms) $ mkStdGen seed

runConcurrent :: [IO a] -> IO [a]
runConcurrent ops = do
  chan <- newChan
  forM_ (zip [0..] ops) $ \(i, op) -> forkIO $ do
    result <- op
    writeChan chan (i, result)
  results <- replicateM (length ops) $ readChan chan
  return $ map snd $ sortOn fst results


runExperiment :: Configuration -> IO ()
runExperiment (Configuration {..}) = 
  writeToFileOrStdout outputFile $ \outHandle -> do
    hSetBuffering outHandle LineBuffering
    hPutStrLn outHandle csvHeaders
    let seeds = expandSeed experimentCount $ seed simulation
    (report0s, state0s) <- execCalls [(Idle, StartSimulation simulation {seed = seed}) | seed <- seeds]
    hPutStrLn outHandle $ reportsToCSVLine 0 report0s
    loop outHandle 0 state0s

  where 
    execCalls statecalls = unzip <$> runConcurrent [execCall state call | (state, call) <- statecalls]

    execCall state call = 
      case runState (processJSONCall call) state of
        (OKReply r, s) -> return (r, s)
        (ErrorReply msg, _) -> do 
          hPutStrLn stderr $ "Simulation error: " ++ msg
          exitFailure
    loop outHandle i states
      | i < runTime = do
        let stepCount = min outputInterval (runTime - i)
        (reports, states') <- execCalls [(state, DoSteps stepCount) | state <- states]
        hPutStrLn outHandle $ reportsToCSVLine (i + stepCount) reports
        when (not $ all (\a ->  infected a || immune a) $ concatMap agents reports) $
          loop outHandle (i + stepCount) states'
      | otherwise = return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> do
      configBytes <- if configFile == "-" then BL.getContents else BL.readFile configFile
      case eitherDecode configBytes of
        Right config -> runExperiment config
        Left err -> do
          hPutStrLn stderr $ "Failed reading config file:\n" ++ err
          exitFailure
    _ -> hPutStrLn stderr $ "Usage: exp-runner CONFIG-FILE\n"

