module UI.Board (drawBoardWithCoords, drawCell, drawStone) where

import Brick
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
import qualified Data.Vector as Vec
import Data.Char (chr)

import Game.Types
import UI.Theme (selectedAttr, cursorAttr)

drawBoardWithCoords :: GameState -> UISelection -> (Int, Int) -> Widget Name
drawBoardWithCoords gs sel cursor =
  let sz = boardSize gs
      colHeaders = hBox $ str "   " : [str $ "  " ++ [chr (c + 65)] ++ " " | c <- [0..sz-1]]
      mkRow r = hBox $ str (rowLabel r) : [drawCell gs r c sel cursor | c <- [0..sz-1]]
      boardRows = [mkRow r | r <- [0..sz-1]]
  in B.borderWithLabel (str " Go Board ") $ vBox (colHeaders : boardRows)

drawCell :: GameState -> Int -> Int -> UISelection -> (Int, Int) -> Widget Name
drawCell gs row col sel (curR, curC) =
  let idx = row * boardSize gs + col
      stone = board gs Vec.! idx
      isSelected = case sel of
                     SelectCoord r c -> r == row && c == col
                     _ -> False
      isCursor = curR == row && curC == col
      cellWidget = str $ " " ++ drawStone stone ++ " "
      
      -- Apply attributes based on selection/cursor state
      styledWidget = if isSelected
                     then withAttr selectedAttr cellWidget
                     else if isCursor
                          then withAttr cursorAttr cellWidget
                          else cellWidget
  in clickable (BoardCell row col) styledWidget

drawStone :: Stone -> String
drawStone Empty = "."
drawStone Black = "●"
drawStone White = "○"

rowLabel :: Int -> String
rowLabel n = if n < 9 then " " ++ show (n + 1) ++ " " else show (n + 1) ++ " "