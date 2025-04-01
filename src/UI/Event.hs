module UI.Event (handleEvent) where

import Brick
import qualified Graphics.Vty as V

import Game.Types
import Game.Engine

-- Combine all the handleEvent patterns into a single function with pattern matching
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