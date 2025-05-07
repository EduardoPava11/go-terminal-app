module UI.Theme 
  ( theMap
  , selectedAttr
  , normalAttr
  , cursorAttr
  , networkAttr
  , peerListAttr
  , activePeerAttr
  , errorAttr
  , connectionInfoAttr
  ) where

import Brick
import Brick.Widgets.Border (borderAttr)
import qualified Graphics.Vty as V

-- Define attribute names
selectedAttr, normalAttr, cursorAttr, networkAttr, peerListAttr, activePeerAttr, errorAttr, connectionInfoAttr :: AttrName
selectedAttr = attrName "selected"
normalAttr = attrName "normal"
cursorAttr = attrName "cursor"
networkAttr = attrName "network"
peerListAttr = attrName "peerList"
activePeerAttr = attrName "activePeer"
errorAttr = attrName "error"
connectionInfoAttr = attrName "connectionInfo"

-- Define the attribute map
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (selectedAttr, V.black `on` V.yellow)
  , (normalAttr, fg V.white)
  , (cursorAttr, V.black `on` V.cyan)
  , (borderAttr, fg V.white)
  , (networkAttr, fg V.green)
  , (peerListAttr, fg V.blue)
  , (activePeerAttr, V.black `on` V.green)
  , (errorAttr, fg V.red)
  , (connectionInfoAttr, fg V.cyan)
  ]