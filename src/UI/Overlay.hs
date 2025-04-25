module UI.Overlay (drawStatus) where

import Brick
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
import qualified Brick.Widgets.Border.Style as BS
import Data.Char (chr)
import UI.Theme

import Game.Types

drawStatus :: UIState -> Widget Name
drawStatus us =
  let gs = gameState us
      (curR, curC) = cursor us
      
      -- Existing status info
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
                
      -- Network status display using the updated Address type
      netStatusStr = case networkStatus us of
                      Disconnected -> "Network: Disconnected"
                      Connecting addr -> "Network: Connecting to " ++ getAddress addr
                      Connected addr peers -> "Network: Connected to " ++ getAddress addr ++ 
                                             " (" ++ show peers ++ " peers)"
                      ConnectionError err -> "Network Error: " ++ err
                      CreatingGame addr -> "Hosting game at " ++ getAddress addr ++ " (waiting for opponent)"
                      JoiningGame gameId addr -> "Joining game " ++ gameId ++ " at " ++ getAddress addr
                      
      -- Connection info display
      connInfoStr = case connectionInfo us of
                     Just info -> "Your connection: " ++ info ++ " (share this code)"
                     Nothing -> ""
      
      -- Style network status based on its value
      netStatusWidget = case networkStatus us of
                         ConnectionError _ -> withAttr networkErrorAttr $ str netStatusStr
                         Connected _ _ -> withAttr networkConnectedAttr $ str netStatusStr 
                         CreatingGame _ -> withAttr networkCreatingAttr $ str netStatusStr
                         _ -> withAttr networkStatusAttr $ str netStatusStr
                         
      -- Only show connection info if available
      connInfoWidget = if null connInfoStr 
                      then emptyWidget
                      else withAttr connectionInfoAttr $ str connInfoStr
  in
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str " Status ") $
    padLeftRight 1 $
    vBox [ str playerStr
         , str selStr
         , str cursorStr
         , str captureStr
         , netStatusWidget
         , connInfoWidget
         ]