module Game.BFS 
  ( findConnectedGroup
  , removeGroup
  ) where

import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Game.Types

-- Find all stones of the same color connected to the stone at index
-- Return both the group and its liberties
findConnectedGroup :: Board -> Stone -> Int -> (S.Set Int, S.Set Int)
findConnectedGroup b color startIdx = 
  bfs b color startIdx S.empty S.empty (Seq.singleton startIdx)

-- BFS implementation to find connected stones
bfs :: Board -> Stone -> Int -> S.Set Int -> S.Set Int -> Seq.Seq Int -> (S.Set Int, S.Set Int)
bfs b color _ visited liberties queue
  | Seq.null queue = (visited, liberties)
  | otherwise = 
      let (idx, rest) = (Seq.index queue 0, Seq.drop 1 queue)
          neighbors = getNeighbors (boardSize' b) idx
          
          -- Partition neighbors into same color, empty (liberties), and other
          (sameColor, emptyOrOther) = S.partition 
                                        (\i -> b V.! i == color) 
                                        (S.fromList neighbors)
          newLiberties = S.filter (\i -> b V.! i == Empty) emptyOrOther
          
          -- Add unvisited same-color stones to queue
          unvisitedSameColor = S.filter (`S.notMember` visited) sameColor
          newQueue = rest <> Seq.fromList (S.toList unvisitedSameColor)
          
          -- Update visited set
          newVisited = S.insert idx visited
      in bfs b color undefined newVisited (S.union liberties newLiberties) newQueue

-- Calculate board size from board vector length (assuming square)
boardSize' :: Board -> Int
boardSize' b = round (sqrt (fromIntegral (V.length b) :: Double))

-- Get neighbor indices (up, down, left, right)
getNeighbors :: Int -> Int -> [Int]
getNeighbors size idx =
  let row = idx `div` size
      col = idx `mod` size
      up    = if row > 0         then Just ((row-1)*size + col) else Nothing
      down  = if row < size-1    then Just ((row+1)*size + col) else Nothing
      left  = if col > 0         then Just (row*size + (col-1)) else Nothing
      right = if col < size-1    then Just (row*size + (col+1)) else Nothing
  in [n | Just n <- [up, down, left, right]]

-- Replace stones in the group with Empty
removeGroup :: S.Set Int -> Board -> Board
removeGroup group b = 
  V.imap (\i stone -> if i `S.member` group then Empty else stone) b