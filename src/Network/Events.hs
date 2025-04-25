{-# LANGUAGE DeriveGeneric #-}

module Network.Events
  ( NetworkEvent(..)
  , broadcastNetworkEvent
  , processNetworkEvents
  ) where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.IORef
import GHC.Generics (Generic)
-- Import everything we need from Game.Types
import Game.Types ( UIState(..), NetworkStatus(..), updateNetworkStatus, 
                   updateConnectionInfo, getNetworkStatus, getAddress,
                   mkConnected, mkCreatingGame, mkJoiningGame, 
                   mkConnectionError )

-- Define network events that can be sent to the UI
data NetworkEvent
  = StatusUpdate NetworkStatus
  | ConnectionInfoUpdate String
  | PeerCountChange Int
  | GameJoined String String  -- Game ID, Node address
  | GameCreated String String  -- Game ID, Node address
  | GameSynced
  | MessageReceived String  -- Message details (for debugging)
  | ErrorOccurred String    -- Error message
  deriving (Show, Generic)

-- Create a function to broadcast network events
broadcastNetworkEvent :: MVar [NetworkEvent] -> NetworkEvent -> IO ()
broadcastNetworkEvent eventQueue event = do
  void $ forkIO $ do
    modifyMVar_ eventQueue (\events -> return (event:events))

-- Process network events and update UI state
processNetworkEvents :: UIState -> [NetworkEvent] -> IO UIState
processNetworkEvents us [] = return us
processNetworkEvents us (event:rest) = do
  newState <- case event of
    StatusUpdate status -> 
      return $ updateNetworkStatus us status
      
    ConnectionInfoUpdate info ->
      return $ updateConnectionInfo us info
      
    PeerCountChange count ->
      case getNetworkStatus us of  -- Use getNetworkStatus instead of networkStatus
        Connected addr _ -> 
          return $ updateNetworkStatus us (mkConnected (getAddress addr) count)
        _ -> return us
        
    GameCreated gameId addr ->
      return $ updateNetworkStatus us (mkCreatingGame addr)
      
    GameJoined gameId addr ->
      return $ updateNetworkStatus us (mkJoiningGame gameId addr)
      
    GameSynced ->
      return us  -- Could add visual feedback here
      
    MessageReceived _ ->
      return us  -- Could log messages for debugging
      
    ErrorOccurred msg ->
      return $ updateNetworkStatus us (mkConnectionError msg)
  
  -- Process remaining events
  processNetworkEvents newState rest