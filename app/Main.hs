module Main where

import Brick
import UI.UI
import qualified Graphics.Vty as V
import Control.Monad (void)

main :: IO ()
main = do
  let sz = 9
      initialState = buildInitialState sz
      app = App
        { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = showFirstCursor
        }
  void $ defaultMain app initialState