{-# LANGUAGE FlexibleContexts #-}

module Network.Thread
  ( startNetworkThread
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Monad (forever)
import Network.Transport (GossipMessage(..))
import Network.Node (Node)
import qualified Network.Membership as Membership
import qualified Network.Gossip as Gossip
import qualified Network.Membership.HyParView as HV
import qualified Network.Gossip.Plumtree as PT
import Control.Concurrent.MVar (MVar)
import Network.Events (NetworkEvent(..), broadcastNetworkEvent)
import Game.Types (NetworkStatus(..))

-- | Start the network thread to process incoming messages
startNetworkThread
  :: (Show Node, Ord Node)
  => HV.Env Node
  -> PT.Env Node
  -> TQueue (GossipMessage Node)
  -> MVar [NetworkEvent]  -- Event queue
  -> IO ()
startNetworkThread hvEnv ptEnv inbox eventQueue = do
    putStrLn "[NetworkThread] Starting combined loop..."
    _ <- forkIO $ forever $ do
      msg <- atomically $ readTQueue inbox
      case msg of
        HVMsg hvRpc -> do
          putStrLn "[NetworkThread] Received HyParView message"
          -- Send an event to the UI
          broadcastNetworkEvent eventQueue (MessageReceived "HyParView message")
          -- MVP stub: bypass actual HV processing for now
          putStrLn "[NetworkThread] Processing HyParView message (MVP stub)"
          Membership.hyParViewLoop hvEnv (return ())
        
        PTMsg ptRpc -> do
          putStrLn "[NetworkThread] Received Plumtree message"
          -- Send an event to the UI
          broadcastNetworkEvent eventQueue (MessageReceived "Plumtree message")
          -- MVP stub: bypass actual PT processing for now
          putStrLn "[NetworkThread] Processing Plumtree message (MVP stub)"
          Gossip.plumtreeLoop ptEnv (return ())
    
    putStrLn "[NetworkThread] Network dispatcher thread started"
    return ()