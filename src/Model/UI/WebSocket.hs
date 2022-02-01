{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, DeriveAnyClass, DeriveGeneric, ExistentialQuantification, ScopedTypeVariables, OverloadedStrings #-}

-- | Runs the JSON RPC protocol via a simple WebSocket server.
--   The client should include add a "session-id" (identifier chosen by client) value to the object in every call. 
--   Session state can be cleared with a special {"call": "cleanup", "session-id": <id>} message. There is no 
--   auto-cleaning or memory limiting functionality. Reset the server to clear everything.
--
--   Clients are trusted. DoS'ing is trivial. Do not expose to the internet.

module Model.UI.WebSocket where

import Model.UI.JSON

import qualified Network.WebSockets as WS
import Control.Monad.Random
import Data.Aeson
import Data.Aeson.Types

import Control.Concurrent.STM
import Control.Exception
import Control.Applicative
import Generics.Deriving (Generic)
import Control.Monad.Trans.State.Strict
import System.IO
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
import qualified Data.ByteString.Lazy.Char8 as BC

defaultPort = 4243
pingInterval = 10

main :: IO ()
main = do
  putStrLn $ "Running WebSocket server at ws://127.0.0.1:" ++ show defaultPort ++ "/.\nOpen ui/app.html in a browser to use the GUI."
  runServer "127.0.0.1" defaultPort

runServer :: String -> Int -> IO ()
runServer host port = do
  sessionStore <- SessionStore <$> newTVarIO M.empty
  WS.runServer host port $ simulationApp sessionStore

type SessionID = String
newtype SessionStore = SessionStore (TVar (Map SessionID (TMVar SimulationState)))

withSession :: SessionStore -> SessionID -> State SimulationState a -> IO a
withSession (SessionStore dbvar) sid action = do
  -- Atomic read-or-create.
  svar <- atomically $ do
    db <- readTVar dbvar
    case M.lookup sid db of
      Just svar -> return svar
      Nothing -> do
        svar <- newTMVar Idle
        modifyTVar dbvar (M.insert sid svar)
        return svar

  -- Only one concurrent action per session.
  bracketOnError (atomically $ takeTMVar svar) (atomically . putTMVar svar) $ \state -> do
    (result, state') <- evaluate $ runState action state
    atomically $ putTMVar svar state'
    return result

cleanSession :: SessionStore -> SessionID -> IO ()
cleanSession (SessionStore dbvar) sid = atomically $ modifyTVar dbvar $ M.delete sid

data WSCall
  = CleanupCall SessionID
  | SimulationCall SessionID Call
  deriving (Show)

instance FromJSON WSCall where
  parseJSON val = 
    (checkCleanup val >> CleanupCall <$> parseSessionID val) 
    <|> (SimulationCall <$> parseSessionID val <*> parseJSON val)
    where
      checkCleanup = withObject "CleanupCall" $ \obj -> do
        (call :: String) <- obj .: "call"
        guard $ call == "cleanup"
      parseSessionID = withObject "CallWithSessionID" (.: "session-id")

simulationApp :: SessionStore -> WS.ServerApp
simulationApp store pending = do
  con <- WS.acceptRequest pending
  WS.withPingThread con pingInterval (return ())
    $ forever (replyToCall con) `catch` (\(_ :: WS.ConnectionException) -> return ()) `catch` errorHandler con
  where
    replyToCall con = do 
      callMsg <- WS.fromDataMessage <$> WS.receiveDataMessage con
      hPutStr stderr "<< " >> BC.hPutStrLn stderr callMsg
      replyMsg <- case eitherDecode' callMsg of
        Left error -> return $ encode $ ErrorReply $ "JSON parse error: " ++ error
        Right(SimulationCall sid call) -> encode <$> (withSession store sid $ processJSONCall call)
        Right (CleanupCall sid) -> do
          cleanSession store sid
          return $ encode $ object ["status" .= ("ok" :: String)]
      hPutStr stderr ">> " >> BC.hPutStrLn stderr (BC.take 100 replyMsg `B.append` BC.pack "...")
      WS.sendTextData con replyMsg
    errorHandler con (ex :: SomeException) = do 
      let msg = "Exception: " ++ show ex
      hPutStrLn stderr msg
      WS.sendTextData con $ encode $ ErrorReply msg