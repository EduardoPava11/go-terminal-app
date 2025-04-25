module UI.Event (handleEvent, handleEventNetwork) where

import Brick
import qualified Graphics.Vty as V
import Control.Monad (void)
import qualified Network.Gossip.Plumtree as PT
import Data.IORef
import Control.Concurrent.MVar (MVar, tryTakeMVar, readMVar, putMVar)
import Network.Events (NetworkEvent(..), processNetworkEvents) -- Import processNetworkEvents

import Game.Types
import Game.Engine
import Network.Node (Node)
import Network.GameState (NetworkGame, applyNetworkMove)

-- Original handleEvent for local play
handleEvent :: UIState -> BrickEvent Name e -> EventM Name (Next UIState)
-- Escape key
handleEvent us (VtyEvent (V.EvKey V.KEsc [])) = halt us

-- Arrow keys and Enter
handleEvent us (VtyEvent (V.EvKey key [])) =
  case key of
    V.KUp    -> continue $ us { cursor = move (-1)  0 }
    V.KDown  -> continue $ us { cursor =  move  1   0 }
    V.KLeft  -> continue $ us { cursor =  move  0 (-1) }
    V.KRight -> continue $ us { cursor =  move  0   1 }
    V.KEnter -> handleEnterKey us
    _        -> continue us
  where
    (r, c) = cursor us
    sz = boardSize (gameState us)
    move dr dc = let r' = max 0 (min (sz - 1) (r + dr))
                     c' = max 0 (min (sz - 1) (c + dc))
                 in (r', c')

-- Mouse clicks on board cells
handleEvent us (MouseDown (BoardCell r c) _ _ _) =
  let sel = case selection us of
              SelectCoord r' c' | r == r' && c == c' -> NoSelection
              _ -> SelectCoord r c
  in continue $ us { selection = sel, cursor = (r, c) }

-- Mouse clicks on Pass button
handleEvent us (MouseDown PassButton _ _ _) =
  let sel = if selection us == SelectPass then NoSelection else SelectPass
  in continue $ us { selection = sel }

-- Mouse clicks on Resign button
handleEvent us (MouseDown ResignButton _ _ _) =
  let sel = if selection us == SelectResign then NoSelection else SelectResign
  in continue $ us { selection = sel }

-- Default case for any other events
handleEvent us _ = continue us

-- Helper function for Enter key logic
handleEnterKey :: UIState -> EventM Name (Next UIState)
handleEnterKey us =
  case selection us of
    NoSelection -> 
      let (r, c) = cursor us
      in continue $ us { selection = SelectCoord r c }
    SelectCoord r c ->
      case applyMove (Place r c) (gameState us) of
        Left _     -> continue us  -- Invalid move
        Right newG -> continue us { gameState = newG, selection = NoSelection }
    SelectPass ->
      case applyMove Pass (gameState us) of
        Left _     -> continue us
        Right newG -> continue us { gameState = newG, selection = NoSelection }
    SelectResign -> halt us  -- Placeholder: could show overlay

-- Network version of handleEvent that broadcasts moves
handleEventNetwork :: PT.Env Node -> IORef NetworkGame -> MVar [NetworkEvent] -> UIState -> BrickEvent Name e -> EventM Name (Next UIState)
handleEventNetwork ptEnv ngRef eventQueue us event = do
  -- Check for network events and update UI accordingly
  networkEvents <- liftIO $ do
    maybeTaken <- tryTakeMVar eventQueue
    case maybeTaken of
      Just evs -> do
        -- Put back an empty list
        putMVar eventQueue []
        return evs
      Nothing -> return []
  
  -- Process network events if any
  updatedState <- if null networkEvents
                  then return us
                  else liftIO $ processNetworkEvents us networkEvents
  
  -- Continue with normal event handling using the updated state
  case event of
    -- Handle keyboard events for game play
    -- Escape key
    VtyEvent (V.EvKey V.KEsc []) -> halt updatedState

    -- Arrow keys and Enter
    VtyEvent (V.EvKey key []) ->
      case key of
        V.KUp    -> continue $ updatedState { cursor = move (-1)  0 }
        V.KDown  -> continue $ updatedState { cursor =  move  1   0 }
        V.KLeft  -> continue $ updatedState { cursor =  move  0 (-1) }
        V.KRight -> continue $ updatedState { cursor =  move  0   1 }
        V.KEnter -> handleEnterKeyNetwork ptEnv ngRef updatedState
        _        -> continue updatedState
      where
        (r, c) = cursor updatedState
        sz = boardSize (gameState updatedState)
        move dr dc = let r' = max 0 (min (sz - 1) (r + dr))
                         c' = max 0 (min (sz - 1) (c + dc))
                     in (r', c')

    -- Mouse clicks on board cells
    MouseDown (BoardCell r c) _ _ _ ->
      let sel = case selection updatedState of
                  SelectCoord r' c' | r == r' && c == c' -> NoSelection
                  _ -> SelectCoord r c
      in continue $ updatedState { selection = sel, cursor = (r, c) }

    -- Mouse clicks on Pass button
    MouseDown PassButton _ _ _ ->
      let sel = if selection updatedState == SelectPass then NoSelection else SelectPass
      in continue $ updatedState { selection = sel }

    -- Mouse clicks on Resign button
    MouseDown ResignButton _ _ _ ->
      let sel = if selection updatedState == SelectResign then NoSelection else SelectResign
      in continue $ updatedState { selection = sel }

    -- Add networking-specific events
    AppEvent NetworkConnected -> do
      -- Update UI when network connection is established
      continue $ updateNetworkStatus updatedState (Connected "connected" 1)
      
    AppEvent NetworkDisconnected -> do
      -- Update UI when network connection is lost
      continue $ updateNetworkStatus updatedState Disconnected

    -- Default handler for all other events
    _ -> handleEvent updatedState event

-- Helper function for Enter key logic with network
handleEnterKeyNetwork :: PT.Env Node -> IORef NetworkGame -> UIState -> EventM Name (Next UIState)
handleEnterKeyNetwork ptEnv ngRef us =
  case selection us of
    NoSelection -> 
      let (r, c) = cursor us
      in continue $ us { selection = SelectCoord r c }
    SelectCoord r c -> do
      -- Get the current NetworkGame state
      ng <- liftIO $ readIORef ngRef
      
      -- Apply the move both locally and broadcast it
      result <- liftIO $ applyNetworkMove ptEnv ng (Place r c)
      
      case result of
        Left _     -> continue us  -- Invalid move
        Right newNg -> do
          -- Update the NetworkGame reference
          liftIO $ writeIORef ngRef newNg
          
          -- Update the UI state with the new game state
          continue $ us { gameState = ngGameState newNg, selection = NoSelection }
    
    SelectPass -> do
      -- Similar to Place, but for Pass move
      ng <- liftIO $ readIORef ngRef
      result <- liftIO $ applyNetworkMove ptEnv ng Pass
      
      case result of
        Left _     -> continue us
        Right newNg -> do
          liftIO $ writeIORef ngRef newNg
          continue $ us { gameState = ngGameState newNg, selection = NoSelection }
    
    SelectResign -> halt us  -- Could be enhanced to broadcast resign