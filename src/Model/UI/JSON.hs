{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, ExistentialQuantification, ScopedTypeVariables, OverloadedStrings, StandaloneDeriving #-}

-- | Simple JSON-based RPC interface to run propagation simulations, can be run using some transport protocol that 
--   allows sending JSON objects back and forth.
--   Start simulation with {"call": "start", "model": <model-type>, "agents": <agent-count>, "seed": <rng-seed>, params": <model-data>} 
--   where model-type is a string; options are hard-coded in supportedModels. 
--   Simulate n steps with {"call": "steps", "amount": <n>}.
--   Result contains {"status": "ok", "report": <report>}. The report format does not depend on the model type.
module Model.UI.JSON where

import Model.RNG
import Model.Definitions
import Model.Mobility.BSA (BSAModel)
import Model.Mobility.SMOOTH (SMOOTHModel)
import Model.Mobility.LevyWalk (LevyWalkModel)
import Model.Identification.Discovery (DiscoveryIdModel)
import Model.Identification.PeriodicTrigger (PeriodicTriggerDiscoveryMethod)
import Model.Identification.Broadcast (BroadcastModel)
import Model.Identification.Dummy (DummyIdModel)
import Model.Identification.Signal (SignalIdModel)
import Model.Infection.Simple (SimpleInfectionModel)
import Model.Infection.Serial (SerialInfectionModel)

import Control.Monad.Random

import Generics.Deriving (Generic)
import Data.Random.Normal
import Control.Monad.Random
import Data.Aeson
import Data.Aeson.Types
import Data.KdMap.Dynamic (KdMap)
import qualified Data.KdMap.Dynamic as KDM

import Data.Maybe
import Data.Proxy
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import System.Random
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Control.Monad.Trans.State.Strict as SM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

data SimulationParams 
  = forall mob id inf. (MobilityModel mob, IdentificationModel id, InfectionModel inf)
    => SimulationParams {agentCount :: Int, seed :: Int, modelParams :: PropagationModel mob id inf}

data Call 
  = StartSimulation SimulationParams
  | DoSteps {stepCount :: Int}

data Reply = OKReply SimulationReport
           | ErrorReply String
           deriving (Show)

instance Show Call where
  show (StartSimulation (SimulationParams count seed params)) = "StartSimulation " ++ show count ++ " " ++ show seed ++ " " ++ show params
  show (DoSteps count) = "DoSteps " ++ show count


data ModelNames = ModelNames {
  mobilityModel       :: String,
  identificationModel :: String,
  infectionModel      :: String
} deriving (Show, Generic, FromJSON)


data MobModelProxy = forall m . MobilityModel m => MobModelProxy (Proxy m)
data IdModelProxy = forall m . IdentificationModel m => IdModelProxy (Proxy m)
data InfModelProxy = forall m. InfectionModel m => InfModelProxy (Proxy m)
data PropModelProxy = forall a b c. (MobilityModel a, IdentificationModel b, InfectionModel c) 
                    => PropModelProxy (Proxy (PropagationModel a b c))

mobModels :: [(String, MobModelProxy)]
mobModels = [
  ("bsa", MobModelProxy (Proxy :: Proxy BSAModel)),
  ("smooth", MobModelProxy (Proxy :: Proxy SMOOTHModel)),
  ("levy", MobModelProxy (Proxy :: Proxy LevyWalkModel))
 ]

type DiscoveryWithBroadcast m = ParallelModels (DiscoveryIdModel m) BroadcastModel
type DiscoveryWithDoubleBroadcast m = ParallelModels3 (DiscoveryIdModel m) BroadcastModel BroadcastModel

idModels :: [(String, IdModelProxy)]
idModels = [
  ("dummy", IdModelProxy (Proxy :: Proxy DummyIdModel)),
  ("signal", IdModelProxy (Proxy :: Proxy SignalIdModel)),
  ("trigger", IdModelProxy (Proxy :: Proxy (DiscoveryIdModel PeriodicTriggerDiscoveryMethod))),
  ("trigger+broadcast", IdModelProxy (Proxy :: Proxy (DiscoveryWithBroadcast PeriodicTriggerDiscoveryMethod))),
  ("trigger+broadcast2", IdModelProxy (Proxy :: Proxy (DiscoveryWithDoubleBroadcast PeriodicTriggerDiscoveryMethod)))
 ]

infModels :: [(String, InfModelProxy)]
infModels = [
  ("simple", InfModelProxy (Proxy :: Proxy SimpleInfectionModel)),
  ("serial", InfModelProxy (Proxy :: Proxy SerialInfectionModel))
 ]


modelByNames :: ModelNames -> Maybe PropModelProxy
modelByNames (ModelNames {..}) = do 
  MobModelProxy (Proxy :: Proxy mob) <- lookup mobilityModel mobModels
  IdModelProxy  (Proxy :: Proxy id)  <- lookup identificationModel idModels
  InfModelProxy (Proxy :: Proxy inf) <- lookup infectionModel infModels
  return $ PropModelProxy (Proxy :: Proxy (PropagationModel mob id inf))

    
data SimulationReport = SimulationReport {
  step   :: Int,
  agents :: [AgentReport]
} deriving (Show, Generic, ToJSON)

instance FromJSON Call where
  parseJSON val = withObject "Call" objParser val
    where 
      objParser obj = do
        calltype <- obj .: "call"
        case calltype of
          "start" -> StartSimulation <$> parseJSON val
          "steps" -> DoSteps <$> obj .: "amount"
          _ -> fail $ "Unkown RPC: " ++ calltype

deriving instance Show SimulationParams

instance FromJSON SimulationParams where
  parseJSON = withObject "SimulationParams" $ \obj -> do
    modelNames <- obj .: "models"
    case modelByNames modelNames of
      Just (PropModelProxy (_ :: Proxy (PropagationModel a b c))) -> 
        SimulationParams <$> obj .: "agents" 
                         <*> obj .: "seed" 
                         <*> (obj .: "params" :: Parser (PropagationModel a b c))
      Nothing -> fail $ "Unknown model names: " ++ show modelNames

instance ToJSON Reply where
  toJSON (OKReply report) = object ["status" .= ("ok" :: String), "report" .= report]
  toJSON (ErrorReply msg) = object ["status" .= ("error" :: String), "message" .= msg]

data SimulationState 
  = Idle
  | forall mob id inf. (MobilityModel mob, IdentificationModel id, InfectionModel inf)
    => Running Int [AgentID] StdGen (State (PropagationModel mob id inf))

stateReport :: (MobilityModel mob, IdentificationModel id, InfectionModel inf) 
            => Int -> [AgentID] -> State (PropagationModel mob id inf) -> SimulationReport
stateReport step agents state = SimulationReport step $ map (\a -> agentReport a state) agents

processJSONCall :: Call -> SM.State SimulationState Reply
processJSONCall (StartSimulation (SimulationParams acount seed m)) = do
  let agents = (map AgentID [0 .. acount - 1])
  let (mstate, rng) = runRNG (initModel agents m) (mkStdGen seed)
  SM.put $ Running 0 agents rng mstate
  return $ OKReply $ stateReport 0 agents mstate
processJSONCall (DoSteps todo) | todo >= 0 = do 
  sstate <- SM.get
  case sstate of
    Idle -> return $ ErrorReply "Simulation not yet started."
    Running done agents rng mstate -> do
      let (mstate', rng') = runRNG (foldrM stepModel mstate $ replicate todo ()) rng
      let totalSteps = done + todo
      SM.put $ Running totalSteps agents rng' mstate'
      return $ OKReply $ stateReport totalSteps agents mstate'

