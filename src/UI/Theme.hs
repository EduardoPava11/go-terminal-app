module UI.Theme 
  ( selectedAttr
  , normalAttr
  , cursorAttr
  , networkStatusAttr
  , networkConnectedAttr
  , networkErrorAttr
  , theMap
  , unicodeBold
  , connectionInfoAttr
  , networkCreatingAttr
  ) where

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

networkStatusAttr, networkConnectedAttr, networkErrorAttr, connectionInfoAttr, networkCreatingAttr :: AttrName
networkStatusAttr = attrName "networkStatus"
networkConnectedAttr = attrName "networkConnected"
networkErrorAttr = attrName "networkError"
connectionInfoAttr = attrName "connectionInfo"
networkCreatingAttr = attrName "networkCreating"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (selectedAttr, V.withForeColor V.defAttr V.red)
  , (normalAttr, V.withForeColor V.defAttr V.white)
  , (cursorAttr, V.withBackColor V.defAttr V.blue)
  , (networkStatusAttr, V.withForeColor V.defAttr V.yellow)
  , (networkConnectedAttr, V.withForeColor V.defAttr V.green)
  , (networkErrorAttr, V.withStyle (V.withForeColor V.defAttr V.red) V.bold)
  , (connectionInfoAttr, V.withStyle (V.withForeColor V.defAttr V.cyan) V.bold)
  , (networkCreatingAttr, V.withForeColor V.defAttr V.magenta)
  ]