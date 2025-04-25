module Game.Engine
  ( initialGameState
  , applyMove
  ) where

import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Game.Types
import Game.BFS

initialGameState :: Int -> GameState
initialGameState size = GameState
  { board = V.replicate (size * size) Empty
  , boardSize = size
  , currentPlayer = Black  -- Black goes first
  , moveNumber = 1
  , capturedBlack = 0
  , capturedWhite = 0
  }

-- Apply a move and update the game state, including captures
applyMove :: Move -> GameState -> Either String GameState
applyMove Pass gs =
  Right gs { currentPlayer = switchPlayer (currentPlayer gs)
           , moveNumber    = moveNumber gs + 1
           }

applyMove Resign _ = Left "Player resigned"

applyMove (Place r c) gs = do
  let sz   = boardSize gs
      idx  = r*sz + c
      player = currentPlayer gs
      opponent = switchPlayer player

  -- 1. Check if move is valid (in bounds, empty spot)
  if not (inBounds r c sz)
    then Left "Move out of bounds"
    else if (board gs V.! idx) /= Empty
      then Left "Spot not empty"
    else do
      -- 2. Tentatively place the stone
      let tentativeBoard = board gs V.// [(idx, player)]

      -- 3. Check for captures of opponent stones
      let neighbors = getNeighbors sz idx
          opponentNeighbors = filter (\i -> tentativeBoard V.! i == opponent) neighbors
          
          -- Find opponent groups adjacent to the new stone
          groupsToRemove = concatMap (findGroupToRemove tentativeBoard opponent) opponentNeighbors
          
          -- Remove captured groups
          (boardAfterCaptures, stonesCaptured) = removeGroups tentativeBoard (S.toList (S.fromList groupsToRemove))

      -- 4. Check for suicide move (only if no captures were made)
      if null groupsToRemove then do
        let (ownGroup, ownLiberties) = findConnectedGroup boardAfterCaptures player idx
        if S.null ownLiberties
          then Left "Suicide move is illegal" -- Disallow suicide
          else Right $ finalizeState gs boardAfterCaptures player stonesCaptured
      else
        -- 5. Finalize state if captures were made (suicide check not needed)
        Right $ finalizeState gs boardAfterCaptures player stonesCaptured

-- Helper to find a single opponent group and check its liberties
findGroupToRemove :: Board -> Stone -> Int -> [Int]
findGroupToRemove b opponentColor startIdx =
  let (group, liberties) = findConnectedGroup b opponentColor startIdx
  in if S.null liberties then S.toList group else []

-- Helper to remove multiple groups and count captured stones
removeGroups :: Board -> [Int] -> (Board, Int)
removeGroups b indices =
  let groupSet = S.fromList indices
      newBoard = V.imap (\i stone -> if i `S.member` groupSet then Empty else stone) b
  in (newBoard, S.size groupSet)

-- Helper to update the GameState after captures
finalizeState :: GameState -> Board -> Stone -> Int -> GameState
finalizeState gs newBoard player stonesCaptured =
  let (newCapturedBlack, newCapturedWhite) =
        if player == Black -- Black just moved, captured White
        then (capturedBlack gs, capturedWhite gs + stonesCaptured)
        else (capturedBlack gs + stonesCaptured, capturedWhite gs) -- White just moved, captured Black
  in gs { board = newBoard
        , currentPlayer = switchPlayer player
        , moveNumber = moveNumber gs + 1
        , capturedBlack = newCapturedBlack
        , capturedWhite = newCapturedWhite
        }

switchPlayer :: Stone -> Stone
switchPlayer Black = White
switchPlayer White = Black
switchPlayer Empty = Black -- Should not happen in normal play

inBounds :: Int -> Int -> Int -> Bool
inBounds r c sz = (r >= 0 && r < sz && c >= 0 && c < sz)