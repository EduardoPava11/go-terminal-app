{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Network.Gossip
  ( initPlumtree
  , startPlumtreeThread
  , plumtreeLoop
  , broadcastGameMessage
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Text as T

import Network.Node (Node)
-- Import the Env type and its fields
import Network.Gossip.Plumtree (Env(..), envConfig, MessageId(..), broadcast)
import qualified Network.Gossip.Plumtree as PT
import Network.Messages (GameMessage(..))
import Network.Config (NodeConfig(..), NodeMode(..))

-- | Initialize the Plumtree protocol
initPlumtree :: NodeConfig -> Node -> IO (Env Node)
initPlumtree config node = do
  env <- PT.new config node
  return env

-- | Start the Plumtree background thread
startPlumtreeThread :: (Show Node) => Env Node -> IO ()
startPlumtreeThread env = do
  putStrLn "[Gossip] Starting Plumtree thread"
  void $ forkIO $ plumtreeLoop env (return ())
  putStrLn "[Gossip] Plumtree thread started"

-- | Main loop for Plumtree - MVP stub implementation
plumtreeLoop :: (Show Node) => Env Node -> PT.Plumtree Node () -> IO ()
plumtreeLoop env action = do
  -- MVP implementation - just log and return
  putStrLn "[Gossip] Plumtree processing (MVP stub)"
  return ()

-- Add this function to enable/disable broadcasts based on node mode
shouldBroadcast :: GameMessage -> NodeConfig -> Bool
shouldBroadcast msg config =
  case nodeMode config of
    Gaming -> True  -- Gaming nodes broadcast everything
    Spectating -> 
      -- Spectating nodes don't initiate game actions
      case msg of
        MsgCreateGame {} -> False
        MsgJoinGame {} -> False
        MsgMove {} -> False
        MsgSyncRequest {} -> True
        MsgSyncResponse {} -> False
    Combined -> True  -- Combined nodes broadcast everything

-- | Broadcast a game message to all nodes
broadcastGameMessage :: Env Node -> GameMessage -> IO ()
broadcastGameMessage env gameMsg = do
  -- Updated to access envConfig directly
  let config = envConfig env
  
  -- Check if this node should broadcast this message type
  if shouldBroadcast gameMsg config then do
    -- Create a message ID
    randomUUID <- UUID.nextRandom
    let msgId = MessageId (T.pack $ show randomUUID)
    
    -- Encode the message
    let payload = BL.toStrict $ encode gameMsg
    
    -- Broadcast
    _ <- PT.runPlumtree env $ broadcast msgId payload
    when (verboseLogging config) $
      putStrLn $ "[Gossip] Broadcasting game message: " ++ show gameMsg
  else
    when (verboseLogging config) $
      putStrLn $ "[Gossip] Skipping broadcast in spectator mode: " ++ show gameMsg