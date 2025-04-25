{-# LANGUAGE OverloadedStrings #-}
module Mocks
  ( mockNode
  , mockConfig
  , mockGameState
  , mockUIState
  , mockNetworkGame
  ) where

import Network.Node (Node(..))
import Network.Config (NodeConfig(..), NodeMode(..))
import Game.Types (GameState(..), Stone(..), UIState(..))
import Network.GameState (NetworkGame(..))
import qualified Data.Vector as V
import qualified Data.Text as T

-- Mock node for testing
mockNode :: Node
mockNode = Node 
  { nodeId = "test-node"
  , nodeAddr = "127.0.0.1:9001"
  }

-- Mock configuration for testing
mockConfig :: NodeConfig
mockConfig = NodeConfig
  { maxActivePeers = 3
  , maxPassivePeers = 10
  , shuffleInterval = 5.0
  , gossipFanout = 2
  , gossipEagerRatio = 0.6
  , messageTimeout = 1.0
  , nodeMode = Gaming
  , hostAddress = "127.0.0.1:9001"
  , nodeId = Just "test-node"
  , verboseLogging = True
  }

-- Mock game state for testing
mockGameState :: GameState
mockGameState = GameState
  { board = V.replicate (9 * 9) Empty
  , boardSize = 9
  , currentPlayer = Black
  , moveNumber = 1
  , capturedBlack = 0
  , capturedWhite = 0
  }

-- Mock UI state for testing
mockUIState :: UIState
mockUIState = UIState
  { gameState = mockGameState
  , selection = NoSelection
  , cursor = (4, 4)
  , networkStatus = mkDisconnected
  , connectionInfo = Nothing
  }

-- Mock network game for testing
mockNetworkGame :: NetworkGame
mockNetworkGame = NetworkGame
  { ngGameId = "test-game-id"
  , ngBlack = mockNode
  , ngWhite = Nothing
  , ngGameState = mockGameState
  , ngLocalPlayer = Black
  }