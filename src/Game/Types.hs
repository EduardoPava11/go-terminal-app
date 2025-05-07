{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Game.Types
  ( -- Core game types
    GameId(..)
  , GameHash
  , Stone(..)
  , Coord
  , GameState(..)
  , GameDelta(..)
  , Move(..)
  , Point(..)
  , Board
  , GameConfig(..)
  , GameResult(..)
  -- UI types
  , UIState(..)
  , UISelection(..)
  , Name(..)
  , NetworkStatus(..)
  , Address(..)
  -- Export accessor functions
  , getNetworkStatus
  -- Export helper functions
  , mkConnected
  , mkConnecting
  , mkDisconnected
  , mkCreatingGame
  , mkJoiningGame
  , mkConnectionError
  , updateNetworkStatus
  , updateConnectionInfo
  , updatePeers
  , updateOpponent
  , buildInitialState
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.Types (ToJSONKey(..), FromJSONKey(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Point on the board (x, y coordinates)
data Point = Point 
  { pointX :: Int
  , pointY :: Int 
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Point
instance FromJSON Point

-- Manual ToJSONKey/FromJSONKey instances for Point
instance ToJSONKey Point
instance FromJSONKey Point

-- | Board representation - 1D vector of length (size*size)
type Board = Vector Stone

-- | Game configuration
data GameConfig = GameConfig 
  { gcBoardSize :: Int   -- Size of the board (19x19, 13x13, 9x9, etc.)
  , gcKomi      :: Float -- Compensation points for the second player
  } deriving (Show, Eq, Generic)

instance ToJSON GameConfig
instance FromJSON GameConfig

-- | Game result representation
data GameResult 
  = Winner Stone  -- Black or White won
  | Draw          -- Draw (equal points)
  deriving (Show, Eq, Generic)

instance ToJSON GameResult
instance FromJSON GameResult

-- | Uniquely identifies a game
newtype GameId = GameId ByteString
  deriving stock (Eq, Ord, Show, Generic)

-- Manual JSON instances for GameId
instance ToJSON GameId where
  toJSON (GameId bs) = toJSON (TE.decodeUtf8 bs)

instance FromJSON GameId where
  parseJSON v = GameId . TE.encodeUtf8 <$> parseJSON v

-- | Game hash type (for network synchronization)
type GameHash = String

-- | Board coordinate (row, column)
type Coord = (Int, Int)

-- | Stone types
data Stone = Empty | Black | White
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Manual ToJSONKey/FromJSONKey instances for Stone
instance ToJSONKey Stone
instance FromJSONKey Stone

-- | Game moves
data Move
  = Play Coord
  | Pass
  | Resign
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Game state
data GameState = GameState
  { gsId         :: GameId           -- Unique game identifier
  , gsBoardSize  :: Int              -- Size of the board (e.g., 19 for 19x19)
  , gsStones     :: Map Coord Stone  -- Stones on the board
  , gsNextPlayer :: Stone            -- Whose turn is it
  , gsHash       :: GameHash         -- Hash of this state
  -- Original fields for compatibility
  , board :: Board                   -- Board as a 1D vector
  , boardSize :: Int                 -- Size of the board (e.g., 19 for 19x19)
  , currentPlayer :: Stone           -- Whose turn is it
  , moveNumber :: Int                -- Current move number
  , capturedBlack :: Int             -- Number of black stones captured
  , capturedWhite :: Int             -- Number of white stones captured
  -- Additional fields needed by Engine.hs
  , gameConfig :: GameConfig         -- Game configuration
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Game delta for network transmission
data GameDelta = GameDelta
  { gdParent :: GameHash   -- Hash of parent state
  , gdMove   :: Move       -- Move to apply
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | UI selection types
data UISelection
  = NoSelection
  | SelectCoord Int Int
  | SelectPass
  | SelectResign
  deriving (Show, Eq)

-- | UI board element names
data Name
  = UIBoardCell Int Int
  | PassButton
  | ResignButton
  deriving (Show, Eq, Ord)

-- | Network address wrapper
newtype Address = Address { getAddress :: String }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Network connection status
data NetworkStatus 
  = Disconnected
  | Connecting Address
  | Connected Address Int        -- Address, peer count
  | ConnectionError String
  | CreatingGame Address
  | JoiningGame String Address   -- Game ID, Address
  deriving (Show, Eq)

-- | UI state
data UIState = UIState
  { gameState :: GameState
  , selection :: UISelection
  , cursor :: (Int, Int)           -- (row, column)
  , networkStatus :: NetworkStatus
  , connectionInfo :: Maybe String  -- Connection info to share
  , peers :: Set Text               -- Connected peer IDs
  , opponent :: Maybe Text          -- Current opponent's peer ID
  , errorMessage :: Maybe Text      -- Error message to display
  } deriving (Show)

-- Smart constructors with validation
mkDisconnected :: NetworkStatus
mkDisconnected = Disconnected

mkConnecting :: String -> NetworkStatus
mkConnecting addr = Connecting (Address addr)

mkConnected :: String -> Int -> NetworkStatus
mkConnected addr peerCount = 
  if peerCount < 0 
  then ConnectionError "Invalid peer count"
  else Connected (Address addr) peerCount

mkConnectionError :: String -> NetworkStatus
mkConnectionError = ConnectionError

mkCreatingGame :: String -> NetworkStatus
mkCreatingGame addr = CreatingGame (Address addr)

mkJoiningGame :: String -> String -> NetworkStatus
mkJoiningGame gameId addr = JoiningGame gameId (Address addr)

-- Accessor functions
getNetworkStatus :: UIState -> NetworkStatus
getNetworkStatus = networkStatus

-- Update functions
updateNetworkStatus :: UIState -> NetworkStatus -> UIState
updateNetworkStatus us status = us { networkStatus = status }

updateConnectionInfo :: UIState -> String -> UIState
updateConnectionInfo us info = us { connectionInfo = Just info }

updatePeers :: UIState -> Set Text -> UIState
updatePeers us peerSet = us { peers = peerSet }

updateOpponent :: UIState -> Maybe Text -> UIState
updateOpponent us opp = us { opponent = opp }

-- | Create initial UI state with an empty board
buildInitialState :: Int -> UIState
buildInitialState sz = UIState
  { gameState = GameState
      { board = V.replicate (sz * sz) Empty
      , boardSize = sz
      , currentPlayer = Black
      , moveNumber = 1
      , capturedBlack = 0
      , capturedWhite = 0
      , gsId = error "Initial GameId not set"
      , gsBoardSize = sz
      , gsStones = M.empty
      , gsNextPlayer = Black
      , gsHash = ""
      , gameConfig = GameConfig sz 6.5  -- Standard komi
      }
  , selection = NoSelection
  , cursor = (0, 0)
  , networkStatus = Disconnected
  , connectionInfo = Nothing
  , peers = S.empty
  , opponent = Nothing
  , errorMessage = Nothing
  }