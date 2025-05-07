module Game.BFS
  ( findConnectedGroup
  ) where

import qualified Data.Vector as V
import qualified Data.Set as S

import Game.Types (Stone(..), Board)

-- | Find a connected group of stones of the same color using BFS
findConnectedGroup :: Board -> Stone -> Int -> (S.Set Int, S.Set Int)
findConnectedGroup board stone startIndex = bfs [startIndex] S.empty S.empty
  where
    size :: Int
    size = round $ sqrt $ (fromIntegral $ V.length board :: Double)
    
    -- Get neighboring indices
    getNeighbors idx =
      let x = idx `mod` size
          y = idx `div` size
          candidates = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
          validCoords = filter (\(nx, ny) -> 
                                   nx >= 0 && nx < size && 
                                   ny >= 0 && ny < size) candidates
      in map (\(nx, ny) -> ny * size + nx) validCoords
    
    -- BFS traversal
    bfs [] group liberties = (group, liberties)
    bfs (idx:queue) group liberties
      | idx `S.member` group = bfs queue group liberties
      | otherwise =
          let neighborIndices = getNeighbors idx
              
              -- Get neighbors with same stone color (to expand group)
              sameColorNeighbors = filter (\i -> i < V.length board && 
                                               board V.! i == stone) 
                                   neighborIndices
              
              -- Get empty neighbors (liberties)
              emptyNeighbors = filter (\i -> i < V.length board && 
                                          board V.! i == Empty)
                               neighborIndices
              
              -- Update sets
              newGroup = S.insert idx group
              newLiberties = S.union liberties (S.fromList emptyNeighbors)
              
              -- Add same color neighbors to queue
              newQueue = queue ++ filter (`S.notMember` newGroup) sameColorNeighbors
          in
            bfs newQueue newGroup newLiberties