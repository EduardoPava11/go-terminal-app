module Game.Types where

import qualified Data.Vector as V

data Stone = Black | White | Empty
  deriving (Eq, Show)

data Move = Place Int Int | Pass | Resign
  deriving (Eq, Show)

type Board = V.Vector Stone

data GameState = GameState
  { board :: Board
  , boardSize :: Int
  , currentPlayer :: Stone
  , moveNumber :: Int
  , capturedBlack :: Int
  , capturedWhite :: Int
  } deriving (Show)

-- UI-specific types
data Name = BoardCell Int Int  -- row, col
          | PassButton
          | ResignButton
          deriving (Ord, Eq, Show)

-- New types for UI interaction
data UISelection
  = NoSelection
  | SelectCoord Int Int    -- row, col
  | SelectPass
  | SelectResign
  deriving (Eq, Show)

data UIState = UIState
  { gameState :: GameState
  , selection :: UISelection
  , cursor :: (Int, Int)   -- row, col
  } deriving (Show)