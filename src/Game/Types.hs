{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types 
  ( -- Export existing types
    UIState(..)
  , GameState(..)
  , Move(..)
  , Stone(..)
  , Player      -- Add type synonym
  , UISelection(..)
  , Selection   -- Add type synonym  
  , BoardCell
  , Board
  , Name(..)
  , NetworkStatus(..)
  , Address
  -- Export accessor functions
  , getNetworkStatus
  -- Export helper functions
  , mkConnected
  , mkConnecting
  , mkDisconnected
  , mkCreatingGame
  , mkJoiningGame
  , mkConnectionError
  , getAddress
  , updateNetworkStatus
  , updateConnectionInfo
  , buildInitialState  -- Make sure this is exported
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.Types (object, withObject)
import Data.Aeson.KeyMap ((.=), (.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T

-- Define type aliases for backward compatibility
type Player = Stone       -- Player is now a Stone
type Selection = UISelection  -- Selection is now UISelection
type BoardCell = Int      -- Simple alias for board cell index

-- A point on the board
data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Point
instance FromJSON Point

-- Different types of stones
data Stone = Empty | Black | White
  deriving (Show, Eq, Generic)

instance ToJSON Stone
instance FromJSON Stone

-- The board is a vector of stones
type Board = Vector Stone

-- A move in the game
data Move 
  = Place Int Int  -- row, column
  | Pass
  | Resign
  deriving (Show, Eq, Generic)

instance ToJSON Move
instance FromJSON Move

-- UI selection types
data UISelection
  = NoSelection
  | SelectCoord Int Int
  | SelectPass
  | SelectResign
  deriving (Show, Eq)

-- UI board element names
data Name
  = BoardCell Int Int
  | PassButton
  | ResignButton
  deriving (Show, Eq, Ord)

-- | A newtype for network addresses with validation
newtype Address = Address { getAddress :: String }
  deriving (Show, Eq, Generic)

instance ToJSON Address
instance FromJSON Address

-- | Create an Address with optional validation
mkAddress :: String -> Address
mkAddress addr = 
  -- Here you could add validation if needed
  Address addr

-- | Network status with smart constructor support
data NetworkStatus 
  = Disconnected
  | Connecting Address           -- ^ Connecting to the given address
  | Connected Address Int        -- ^ Connected with given peer count
  | ConnectionError String       -- ^ An error occurred
  | CreatingGame Address         -- ^ Hosting a game at given address
  | JoiningGame String Address   -- ^ Joining a game with ID at address
  deriving (Show, Eq)

-- Smart constructors with validation
mkDisconnected :: NetworkStatus
mkDisconnected = Disconnected

mkConnecting :: String -> NetworkStatus
mkConnecting addr = Connecting (mkAddress addr)

mkConnected :: String -> Int -> NetworkStatus
mkConnected addr peers = 
  if peers < 0 
  then ConnectionError "Invalid peer count"
  else Connected (mkAddress addr) peers

mkConnectionError :: String -> NetworkStatus
mkConnectionError = ConnectionError

mkCreatingGame :: String -> NetworkStatus
mkCreatingGame addr = CreatingGame (mkAddress addr)

mkJoiningGame :: String -> String -> NetworkStatus
mkJoiningGame gameId addr = JoiningGame gameId (mkAddress addr)

-- UI state
data UIState = UIState
  { gameState :: GameState
  , selection :: UISelection
  , cursor :: (Int, Int)  -- row, column
  , networkStatus :: NetworkStatus
  , connectionInfo :: Maybe String  -- Connection info to share
  } deriving (Show)

-- Add accessor function explicitly with different name
getNetworkStatus :: UIState -> NetworkStatus
getNetworkStatus = networkStatus

-- Update function using smart constructors
updateNetworkStatus :: UIState -> NetworkStatus -> UIState
updateNetworkStatus us status = us { networkStatus = status }

updateConnectionInfo :: UIState -> String -> UIState
updateConnectionInfo us info = us { connectionInfo = Just info }

-- The full game state
data GameState = GameState
  { board :: Board
  , boardSize :: Int
  , currentPlayer :: Stone
  , moveNumber :: Int
  , capturedBlack :: Int  -- number of black stones captured
  , capturedWhite :: Int  -- number of white stones captured
  } deriving (Show, Generic)

-- Rest of your GameState code remains the same
instance ToJSON GameState where
    toJSON gs = object
        [ "board" .= V.toList (board gs)
        , "size" .= boardSize gs
        , "currentPlayer" .= currentPlayer gs
        , "moveNumber" .= moveNumber gs
        , "capturedBlack" .= capturedBlack gs
        , "capturedWhite" .= capturedWhite gs
        ]

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \v -> GameState
        <$> (V.fromList <$> v .: "board")
        <*> v .: "size"
        <*> v .: "currentPlayer"
        <*> v .: "moveNumber"
        <*> v .: "capturedBlack"
        <*> v .: "capturedWhite"

-- Build initial game state
buildInitialState :: Int -> GameState
buildInitialState sz = GameState
  { board = V.replicate (sz * sz) Empty
  , boardSize = sz
  , currentPlayer = Black  -- Black goes first
  , moveNumber = 1
  , capturedBlack = 0
  , capturedWhite = 0
  }