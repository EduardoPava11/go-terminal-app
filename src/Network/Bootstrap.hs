{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Bootstrap
  ( bootstrapNode
  , NetworkComponents(..)
  ) where

import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (void)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.UUID.V4 as UUID

import Network.Config (NodeConfig(..))
import Network.Node (Node(..))
import Network.Transport (GossipMessage)
import qualified Network.Transport as Transport
import qualified Network.Membership as Membership
import qualified Network.Gossip as Gossip
import Network.Thread (startNetworkThread)
import Network.Events (NetworkEvent(..), broadcastNetworkEvent)
import Game.Types (NetworkStatus(..))

-- | Contains all network components initialized by bootstrapNode
data NetworkComponents = NetworkComponents
  { ncNode :: Node
  , ncConfig :: NodeConfig
  , ncHyParViewEnv :: Membership.Env Node
  , ncPlumtreeEnv :: Gossip.Env Node
  , ncInbox :: TQueue (GossipMessage Node)
  , ncEventQueue :: MVar [NetworkEvent]
  }

-- | Bootstrap the network components
bootstrapNode :: NodeConfig    -- ^ Base configuration
             -> String         -- ^ Node identifier argument (if provided)
             -> String         -- ^ Host (e.g. "127.0.0.1")
             -> String         -- ^ Port (e.g. "9001")
             -> Maybe String   -- ^ Optional peer address for joining
             -> MVar [NetworkEvent] -- ^ Event queue for UI updates
             -> IO NetworkComponents
bootstrapNode config nodeIdStr host port maybePeer eventQueue = do
  -- Notify UI that we're connecting
  let connectionAddr = host ++ ":" ++ port
  broadcastNetworkEvent eventQueue (StatusUpdate (mkConnecting connectionAddr))
  
  -- Override config with command line arguments
  let nodeIdValue = maybe (T.pack nodeIdStr) id (nodeId config)
      hostValue = T.pack connectionAddr
      updatedConfig = config { nodeId = Just nodeIdValue, hostAddress = hostValue }
  
  let myNode = Node { nodeId = nodeIdValue, nodeAddr = hostValue }

  putStrLn $ "[Bootstrap] Starting network node " ++ T.unpack nodeIdValue ++ " on " ++ connectionAddr
  putStrLn $ "[Bootstrap] Node mode: " ++ show (nodeMode updatedConfig)

  -- Send connection info to UI
  broadcastNetworkEvent eventQueue (ConnectionInfoUpdate connectionAddr)

  -- Initialize membership and gossip with the config
  putStrLn "[Bootstrap] Initializing HyParView..."
  hvEnv <- Membership.initHyParView updatedConfig myNode

  putStrLn "[Bootstrap] Initializing Plumtree..."
  ptEnv <- Gossip.initPlumtree updatedConfig myNode

  putStrLn "[Bootstrap] Creating inbound TQueue..."
  inbox <- newTQueueIO

  putStrLn $ "[Bootstrap] Starting transport (listen on " ++ port ++ ")..."
  Transport.startTransportSafe inbox (T.pack connectionAddr) `catch` \(e :: SomeException) -> do
    putStrLn $ "[Bootstrap] Transport error: " ++ show e
    broadcastNetworkEvent eventQueue (StatusUpdate (mkConnectionError $ "Transport error: " ++ show e))
    throwIO e  -- Re-throw to terminate bootstrap
  
  -- If there's a peer to connect to, indicate we're joining
  case maybePeer of
    Just peerAddr ->
      broadcastNetworkEvent eventQueue (StatusUpdate (mkConnecting peerAddr))
    Nothing ->
      -- Update UI that we're connected (as host)
      broadcastNetworkEvent eventQueue (StatusUpdate (mkConnected connectionAddr 0))

  -- Start threads with proper error handling
  void $ forkIO $ Membership.startHyParViewThread hvEnv 
                   (maybe [] (\p -> [Node (pack "peer") (pack p)]) maybePeer)
                   `catch` \(e :: SomeException) -> do
                      putStrLn $ "[Bootstrap] Membership error: " ++ show e
                      broadcastNetworkEvent eventQueue 
                        (ErrorOccurred $ "Membership error: " ++ show e)
  
  void $ forkIO $ Gossip.startPlumtreeThread ptEnv
                   `catch` \(e :: SomeException) -> do
                      putStrLn $ "[Bootstrap] Gossip error: " ++ show e
                      broadcastNetworkEvent eventQueue 
                        (ErrorOccurred $ "Gossip error: " ++ show e)
  
  -- Start the network dispatcher with event queue
  void $ forkIO $ startNetworkThread hvEnv ptEnv inbox eventQueue
                   `catch` \(e :: SomeException) -> do
                      putStrLn $ "[Bootstrap] Thread error: " ++ show e
                      broadcastNetworkEvent eventQueue 
                        (ErrorOccurred $ "Thread error: " ++ show e)
  
  return NetworkComponents
    { ncNode = myNode
    , ncConfig = updatedConfig
    , ncHyParViewEnv = hvEnv
    , ncPlumtreeEnv = ptEnv
    , ncInbox = inbox
    , ncEventQueue = eventQueue
    }