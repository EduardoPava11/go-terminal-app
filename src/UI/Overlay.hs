module UI.Overlay (drawStatus) where

import Brick
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
import qualified Brick.Widgets.Border.Style as BS
import Data.Char (chr)

import Game.Types

drawStatus :: UIState -> Widget Name
drawStatus us =
  let gs = gameState us
      (curR, curC) = cursor us
      
      playerStr = case currentPlayer gs of
                    Black -> "Black's turn"
                    White -> "White's turn"
                    _     -> "Game over"

      selStr = case selection us of
                 NoSelection     -> "No selection"
                 SelectCoord r c -> "Selected: " ++ [chr (c + 65)] ++ show (r + 1)
                 SelectPass      -> "Selected: Pass"
                 SelectResign    -> "Selected: Resign"

      cursorStr = "Cursor: " ++ [chr (curC + 65)] ++ show (curR + 1)

      captureStr = "Black captures: " ++ show (capturedWhite gs)
                ++ " | White captures: " ++ show (capturedBlack gs)
  in
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str " Status ") $
    padLeftRight 1 $
    vBox [ str playerStr
         , str selStr
         , str cursorStr
         , str captureStr
         ]