{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Engine
  ( -- * Core Game Types
    GameState(..)
  , GameConfig(..)
  , Move(..)
  , Stone(..)
  , Point(..)
  , GameResult(..)
    -- * Game State Operations
  , newGame
  , initialGameState  -- Alias for emptyBoard
  , makeMove
  , passTurn
  , gameOver
  , getScore
  , getWinner
    -- * Board Query Functions
  , isValidMove
  , getLiberties
  , getNeighbors
  , getEmptyPoints
  , getStoneGroups
  , isGameOver
  , emptyBoard
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.Types
import Game.BFS (findConnectedGroup)

-- Vector update operator
(//) :: V.Vector a -> [(Int, a)] -> V.Vector a
(//) v pairs = V.update v (V.fromList [(i, x) | (i, x) <- pairs])

-- | Create empty board of given size with given player to move
emptyBoard :: Int -> Stone -> GameState
emptyBoard sz player = GameState
  { board = V.replicate (sz * sz) Empty
  , boardSize = sz
  , currentPlayer = player
  , moveNumber = 1
  , capturedBlack = 0
  , capturedWhite = 0
  , gsId = error "Initial GameId not set"
  , gsBoardSize = sz
  , gsStones = M.empty
  , gsNextPlayer = player
  , gsHash = ""
  , gameConfig = GameConfig sz 6.5  -- Standard komi
  }

-- | Alias for emptyBoard to maintain compatibility with existing code
initialGameState :: Int -> GameState
initialGameState = flip emptyBoard Black

-- | Convert a Point to a board index
pointToIndex :: Point -> Int -> Int
pointToIndex (Point x y) size = y * size + x

-- | Find captured stones after a move
-- TODO(clean-warnings): Unused but might be needed in future implementation
_findCapturedStones :: Board -> Int -> Stone -> [Int]
_findCapturedStones board size stone =
  let indices = [i | i <- [0 .. (size*size - 1)], board V.! i == stone]
      groups = _groupConnectedStones board indices
      capturedGroups = filter (_hasNoLiberties board size) groups
  in concat capturedGroups

-- | Group connected stones into groups
-- TODO(clean-warnings): Unused but might be needed in future implementation
_groupConnectedStones :: Board -> [Int] -> [[Int]]
_groupConnectedStones board indices =
  let 
    -- Process all stones, forming groups
    processStones [] groups = groups
    processStones (i:rest) groups
      | any (i `elem`) groups = processStones rest groups
      | otherwise =
          let stone = board V.! i
              (group, _) = findConnectedGroup board stone i
          in processStones rest (S.toList group : groups)
  in
    processStones indices []

-- | Check if a group has no liberties
-- TODO(clean-warnings): Unused but might be needed in future implementation
_hasNoLiberties :: Board -> Int -> [Int] -> Bool
_hasNoLiberties board size group =
  let liberties = concatMap (_getLibertyIndices board size) group
  in null liberties

-- | Get liberties (empty adjacent points) for a stone at given index
-- TODO(clean-warnings): Unused but might be needed in future implementation
_getLibertyIndices :: Board -> Int -> Int -> [Int]
_getLibertyIndices board size idx =
  let neighbors = _getNeighborIndices size idx
  in filter (\i -> i >= 0 && i < V.length board && board V.! i == Empty) neighbors

-- | Get neighboring indices of a given index
-- TODO(clean-warnings): Unused but might be needed in future implementation
_getNeighborIndices :: Int -> Int -> [Int]
_getNeighborIndices size idx =
  let x = idx `mod` size
      y = idx `div` size
      -- Generate neighbors, filtering ones outside board boundaries
      candidates = [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]
      validCoords = filter (\(nx, ny) -> nx >= 0 && nx < size && ny >= 0 && ny < size) candidates
  in map (\(nx, ny) -> ny * size + nx) validCoords

-- | Create a new game with the given configuration
newGame :: Int -> GameState
newGame sz = emptyBoard sz Black

-- | Make a move at the specified point
makeMove :: GameState -> (Int, Int) -> Either String GameState
makeMove state (row, col) = 
  let point = Point col row  -- Note: Point is (x, y) so col=x, row=y
  in if not (isValidMove state point)
     then Left "Invalid move: position is occupied or out of bounds"
     else Right (executeMove state point)

-- | Execute a move, assuming it's valid
executeMove :: GameState -> Point -> GameState
executeMove state point =
  let boardSize' = boardSize state
      currentPlayer' = currentPlayer state
      moveNumber' = moveNumber state
      capturedBlack' = capturedBlack state
      capturedWhite' = capturedWhite state
      board' = board state
      
      idx = pointToIndex point boardSize'
      -- Place stone
      newBoard = board' // [(idx, currentPlayer')]
      -- Check for captures (simplified)
      opponent = if currentPlayer' == Black then White else Black
      captured = []  -- Simplified - no capture logic yet
      -- Update game state
      newState = state { 
          board = newBoard,
          currentPlayer = opponent,
          moveNumber = moveNumber' + 1,
          capturedBlack = if currentPlayer' == White 
                          then capturedBlack' + length captured 
                          else capturedBlack',
          capturedWhite = if currentPlayer' == Black 
                          then capturedWhite' + length captured 
                          else capturedWhite'
      }
  in newState

-- | Pass turn
passTurn :: GameState -> GameState
passTurn state =
  state { currentPlayer = if currentPlayer state == Black then White else Black,
          moveNumber = moveNumber state + 1
        }

-- | Check if a move is valid at the specified point
isValidMove :: GameState -> Point -> Bool
isValidMove state point =
  let boardSize' = boardSize state
      board' = board state
      idx = pointToIndex point boardSize'
      -- Bounds check
      inBounds = pointX point >= 0 && pointX point < boardSize' &&
                 pointY point >= 0 && pointY point < boardSize'
  in
    -- Basic validity check
    inBounds && 
    -- Point must be empty
    (idx < V.length board' && board' V.! idx == Empty) &&
    -- Ko rule check would go here in a more complete implementation
    True

-- | Check if placing a stone would violate the ko rule
-- Placeholder implementation - would need proper history tracking
-- TODO(clean-warnings): Unused but might be needed in future implementation
_wouldViolateKoRule :: Board -> [Board] -> Stone -> Point -> Int -> Bool
_wouldViolateKoRule _ _ _ _ _ = False  -- Simplified implementation

-- | Check if the game is over
-- For simplicity, we'll assume consecutive passes are tracked elsewhere 
-- or we might need to add a field to GameState to track them
isGameOver :: GameState -> Bool
isGameOver _ = False  -- Placeholder - implement proper game over logic

-- | Check if the game is over (using old function name for compatibility)
gameOver :: GameState -> Bool
gameOver = isGameOver

-- | Calculate the score for a player
getScore :: GameState -> Stone -> Float
getScore state player =
  let -- Access fields directly instead of using wildcards
      board' = board state
      capturedWhite' = capturedWhite state
      capturedBlack' = capturedBlack state
      gameConfig' = gameConfig state
      
      -- Count stones on board
      stonesOnBoard = length [i | i <- [0..V.length board'-1], board' V.! i == player]
      
      -- Count captures
      captures = if player == Black then capturedWhite' else capturedBlack'
      
      -- Count territory (simplified - would need full implementation)
      territory = 0
      
      -- Add komi if white
      komi = if player == White then gcKomi gameConfig' else 0
      
  in fromIntegral (stonesOnBoard + captures + territory) + komi

-- | Get the winner of the game, if any
getWinner :: GameState -> Maybe GameResult
getWinner state
  | not (gameOver state) = Nothing  -- Game still in progress
  | blackScore > whiteScore = Just (Winner Black)
  | whiteScore > blackScore = Just (Winner White)
  | otherwise = Just Draw
  where
    blackScore = getScore state Black
    whiteScore = getScore state White

-- | Get connected group of stones of the same color
getConnectedGroup :: Board -> Point -> Int -> [Point]
getConnectedGroup board startPoint size =
  let idx = pointToIndex startPoint size
      stone = board V.! idx
      -- Use findConnectedGroup to get connected stones
      (group, _) = findConnectedGroup board stone idx
  in  -- Convert indices back to Points
      [Point (i `mod` size) (i `div` size) | i <- S.toList group]

-- | Get connected empty points starting from a point
-- TODO(clean-warnings): Unused but might be needed in future implementation
_getConnectedEmptyPoints :: Board -> Point -> Int -> [Point]
_getConnectedEmptyPoints board startPoint size =
  -- Use breadthFirstSearch to find all connected empty points
  _breadthFirstSearch 
    startPoint
    (\p -> filter
        (\n -> let idx = pointToIndex n size
               in idx >= 0 && idx < V.length board && board V.! idx == Empty)
        (getNeighbors (pointX p) (pointY p) size))
    size

-- | BFS implementation to find connected points
-- TODO(clean-warnings): Unused but might be needed in future implementation
_breadthFirstSearch :: Point -> (Point -> [Point]) -> Int -> [Point]
_breadthFirstSearch start getNeighborsFn _ = 
  bfs [start] S.empty
  where
    bfs [] visited = S.toList visited
    bfs (p:queue) visited
      | p `S.member` visited = bfs queue visited
      | otherwise =
          let neighbors = filter (`S.notMember` visited) (getNeighborsFn p)
              newVisited = S.insert p visited
          in bfs (queue ++ neighbors) newVisited

-- | Get all neighboring points of a given position
getNeighbors :: Int -> Int -> Int -> [Point]
getNeighbors x y size =
  filter (\(Point nx ny) -> nx >= 0 && nx < size && ny >= 0 && ny < size)
    [Point (x+1) y, Point (x-1) y, Point x (y+1), Point x (y-1)]

-- | Get liberties for a single point
getLiberties :: GameState -> Point -> [Point]
getLiberties state point =
  let boardSize' = boardSize state
      board' = board state
      neighbors = getNeighbors (pointX point) (pointY point) boardSize'
  in filter (\p -> 
      let idx = pointToIndex p boardSize'
      in idx >= 0 && idx < V.length board' && board' V.! idx == Empty) 
     neighbors

-- | Get all empty points on the board
getEmptyPoints :: GameState -> [Point]
getEmptyPoints state =
  let boardSize' = boardSize state
      board' = board state
      allIndices = [0 .. (boardSize' * boardSize' - 1)]
      emptyIndices = filter (\i -> board' V.! i == Empty) allIndices
  in [Point (i `mod` boardSize') (i `div` boardSize') | i <- emptyIndices]

-- | Find all groups of stones for a given player
getStoneGroups :: GameState -> Stone -> [[Point]]
getStoneGroups state stone =
  let -- Access fields directly instead of using wildcards
      boardSize' = boardSize state  
      board' = board state
      
      -- Get all stones of this color
      indices = [i | i <- [0 .. (boardSize' * boardSize' - 1)], board' V.! i == stone]
      stones = [Point (i `mod` boardSize') (i `div` boardSize') | i <- indices]
      
      -- Group connected stones
      groupStones processed [] = processed
      groupStones processed (s:rest)
        | any (s `elem`) processed = groupStones processed rest
        | otherwise = 
            let group = getConnectedGroup board' s boardSize'
            in groupStones (group : processed) rest
  in
    groupStones [] stones