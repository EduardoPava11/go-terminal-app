{-# LANGUAGE OverloadedStrings #-}
module UI.Overlay
  ( drawOverlay
  , drawStatus
  ) where

import Brick
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C
import Data.Text (Text)
import qualified Data.Text as T

import Game.Types
import UI.Theme

-- | Draw an overlay with connection status and error messages
drawOverlay :: UIState -> Widget Name
drawOverlay us = 
  case (networkStatus us, errorMessage us) of
    (_, Just err) -> 
      -- Draw error overlay
      drawErrorOverlay err
    (Connecting addr, _) ->
      -- Draw connecting overlay
      drawConnectingOverlay addr
    (CreatingGame addr, _) -> 
      -- Draw creating game overlay
      drawCreatingGameOverlay addr
    (JoiningGame code addr, _) ->
      -- Draw joining game overlay
      drawJoiningGameOverlay code addr
    _ -> emptyWidget

-- | Draw status bar with game info
drawStatus :: UIState -> Widget Name
drawStatus us =
  let gs = gameState us
      net = networkStatus us
  in
  B.hBorderWithLabel (str " Status ") <=>
  hBox 
    [ padRight (Pad 2) $ str $ "Turn: " ++ show (currentPlayer gs)
    , padRight (Pad 2) $ str $ "Move: " ++ show (moveNumber gs)
    , padRight (Pad 2) $ str $ "Captures - B: " ++ show (capturedBlack gs) ++ 
                              " W: " ++ show (capturedWhite gs)
    , padRight (Pad 2) $ withAttr networkAttr $ str $ showNetworkStatus net
    ]

-- | Show network status as string
showNetworkStatus :: NetworkStatus -> String
showNetworkStatus Disconnected = "Disconnected"
showNetworkStatus (Connecting (Address addr)) = "Connecting to " ++ addr
showNetworkStatus (Connected (Address addr) n) = "Connected to " ++ addr ++ " (" ++ show n ++ " peers)"
showNetworkStatus (ConnectionError err) = "Connection Error: " ++ err
showNetworkStatus (CreatingGame (Address addr)) = "Creating game on " ++ addr
showNetworkStatus (JoiningGame code (Address addr)) = "Joining game " ++ code ++ " on " ++ addr

-- | Draw error overlay
drawErrorOverlay :: Text -> Widget Name
drawErrorOverlay err =
  C.center $
  withAttr errorAttr $
  B.border $
  padAll 2 $
  vBox [ str "Error"
       , str " "
       , str (T.unpack err)
       , str " "
       , str "Press Esc to continue"
       ]

-- | Draw connecting overlay
drawConnectingOverlay :: Address -> Widget Name
drawConnectingOverlay (Address addr) =
  C.center $
  B.border $
  padAll 2 $
  vBox [ str "Connecting"
       , str " "
       , str $ "Connecting to " ++ addr
       , str " "
       , str "Please wait..."
       ]

-- | Draw creating game overlay
drawCreatingGameOverlay :: Address -> Widget Name
drawCreatingGameOverlay (Address addr) =
  C.center $
  B.border $
  padAll 2 $
  vBox [ str "Creating Game"
       , str " "
       , str $ "Creating game on " ++ addr
       , str " "
       , str "Please wait..."
       ]

-- | Draw joining game overlay
drawJoiningGameOverlay :: String -> Address -> Widget Name
drawJoiningGameOverlay code (Address addr) =
  C.center $
  B.border $
  padAll 2 $
  vBox [ str "Joining Game"
       , str " "
       , str $ "Joining game " ++ code
       , str $ "on " ++ addr
       , str " "
       , str "Please wait..."
       ]