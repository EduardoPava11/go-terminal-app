module UI.UI
  ( drawUI
  , handleEvent
  , buildInitialState
  , theMap
  ) where

import Brick
import Brick.Widgets.Core
import qualified Brick.Widgets.Center as C

import Game.Types
import Game.Engine
import UI.Board
import UI.Overlay
import UI.Buttons
import UI.Event
import UI.Theme

-- Build initial UI state
buildInitialState :: Int -> UIState
buildInitialState size = UIState
  { gameState = initialGameState size
  , selection = NoSelection
  , cursor = (0, 0)  -- Initialize cursor at top-left
  }

-- Main drawing function
drawUI :: UIState -> [Widget Name]
drawUI us = 
  [ C.center $ 
    vBox [ drawBoardWithCoords (gameState us) (selection us) (cursor us)
         , padTop (Pad 1) $ drawStatus us
         , padTop (Pad 1) $ drawButtons (selection us)
         ]
  ]