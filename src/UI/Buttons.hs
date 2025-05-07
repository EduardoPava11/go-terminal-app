module UI.Buttons
  ( drawButtons
  ) where

import Brick
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Game.Types

import UI.Theme

drawButtons :: UISelection -> Widget Name
drawButtons sel =
  let passAttr = if sel == SelectPass then selectedAttr else normalAttr
      resignAttr = if sel == SelectResign then selectedAttr else normalAttr
  in
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str " Actions ") $
    padLeftRight 2 $
    hBox [ clickable PassButton $ withAttr passAttr $ str "[ Pass ]"
         , str "     "
         , clickable ResignButton $ withAttr resignAttr $ str "[ Resign ]"
         ]