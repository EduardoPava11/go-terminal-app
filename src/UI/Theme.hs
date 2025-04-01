module UI.Theme (selectedAttr, normalAttr, cursorAttr, theMap, unicodeBold) where

import Brick
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border.Style as BS

-- Export the unicodeBold style
unicodeBold :: BS.BorderStyle
unicodeBold = BS.unicodeBold

selectedAttr, normalAttr, cursorAttr :: AttrName
selectedAttr = attrName "selected"
normalAttr = attrName "normal"
cursorAttr = attrName "cursor"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (selectedAttr, V.withForeColor V.defAttr V.red)
  , (normalAttr, V.withForeColor V.defAttr V.white)
  , (cursorAttr, V.withBackColor V.defAttr V.blue)
  ]