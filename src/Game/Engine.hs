module Game.Engine
  ( initialGameState
  , applyMove
  ) where

import qualified Data.Vector as V
import qualified Data.Set as S  -- Add this import
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

applyMove :: Move -> GameState -> Either String GameState
applyMove Pass gs =
  Right gs { currentPlayer = switchPlayer (currentPlayer gs)
           , moveNumber    = moveNumber gs + 1
           }

applyMove Resign _ = Left "Player resigned"

applyMove (Place r c) gs = do
  let sz   = boardSize gs
      idx  = r*sz + c
  if not (inBounds r c sz)
    then Left "Move out of bounds"
    else if (board gs V.! idx) /= Empty
      then Left "Spot not empty"
      else Right ()

  let st = currentPlayer gs
      placedBoard = (board gs) V.// [(idx, st)]
      enemyColor  = switchPlayer st
      (capturedBoard, blackCaps, whiteCaps) = removeDeadGroups placedBoard enemyColor sz

  let gsNew = gs { board         = capturedBoard
                 , moveNumber    = moveNumber gs + 1
                 , currentPlayer = enemyColor
                 , capturedBlack = capturedBlack gs + blackCaps
                 , capturedWhite = capturedWhite gs + whiteCaps
                 }
  return gsNew

-- Remove groups of 'enemyColor' that have no liberties
removeDeadGroups :: Board -> Stone -> Int -> (Board, Int, Int)
removeDeadGroups brd color sz =
  foldl checkOne (brd, 0, 0) [0..(V.length brd - 1)]
  where
    checkOne (b, blackC, whiteC) i =
      if b V.! i == color
        then
          let (grp, libs) = findConnectedGroup b color i
          in if S.null libs
               then let removedCnt = S.size grp
                        b2 = removeGroup grp b
                    in case color of
                         Black -> (b2, blackC + removedCnt, whiteC)
                         White -> (b2, blackC, whiteC + removedCnt)  -- FIXED: use b2 
                         Empty -> (b, blackC, whiteC)  -- Cannot capture empty spaces
               else (b, blackC, whiteC)
        else (b, blackC, whiteC)

switchPlayer :: Stone -> Stone
switchPlayer Black = White
switchPlayer White = Black
switchPlayer Empty = Black

inBounds :: Int -> Int -> Int -> Bool
inBounds r c sz = (r >= 0 && r < sz && c >= 0 && c < sz)