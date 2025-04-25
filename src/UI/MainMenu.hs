{-# LANGUAGE OverloadedStrings #-}

module UI.MainMenu
  ( drawMainMenu
  , handleMainMenuEvent
  , MainMenuState(..)
  , MainMenuSelection(..)
  ) where

import Brick
import Brick.Widgets.Core
import Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T

import Game.Types (Name(..))
import UI.Theme

-- Main menu options
data MainMenuSelection
  = CreateGameOption
  | JoinGameOption
  | LocalGameOption
  | QuitOption
  deriving (Eq, Show)

-- Menu state
data MainMenuState = MainMenuState
  { menuSelection :: MainMenuSelection
  , gameCode :: Maybe Text  -- For join game option
  , joinError :: Maybe Text  -- Error message when joining fails
  }

-- Names for menu widgets
data MainMenuName
  = CreateButton
  | JoinButton
  | LocalButton
  | QuitButton
  | GameCodeInput
  deriving (Eq, Ord, Show)

-- Initialize menu state
initialMainMenuState :: MainMenuState
initialMainMenuState = MainMenuState
  { menuSelection = CreateGameOption
  , gameCode = Nothing
  , joinError = Nothing
  }

-- Draw the main menu
drawMainMenu :: MainMenuState -> [Widget MainMenuName]
drawMainMenu ms = 
  [ C.center $ 
    vBox [ B.borderWithLabel (str " Go Terminal App ") $ 
           padAll 2 $ 
           vBox [ str "Welcome to Go Terminal App!"
                , str " "
                , drawMenuOptions (menuSelection ms)
                , str " "
                , case menuSelection ms of
                    JoinGameOption -> 
                      vBox [ str "Enter game code to join:"
                           , drawGameCodeInput (gameCode ms)
                           , maybe emptyWidget (withAttr selectedAttr . str . T.unpack) (joinError ms)
                           ]
                    _ -> emptyWidget
                ]
         ]
  ]

-- Draw menu options
drawMenuOptions :: MainMenuSelection -> Widget MainMenuName
drawMenuOptions sel =
  vBox [ drawOption CreateButton "Create New Game" (sel == CreateGameOption)
       , drawOption JoinButton "Join Existing Game" (sel == JoinGameOption)
       , drawOption LocalButton "Play Local Game" (sel == LocalGameOption)
       , drawOption QuitButton "Quit" (sel == QuitOption)
       ]

-- Draw individual menu option
drawOption :: MainMenuName -> String -> Bool -> Widget MainMenuName
drawOption name label isSelected =
  let attr = if isSelected then selectedAttr else normalAttr
  in clickable name $ withAttr attr $ str $ if isSelected then "> " ++ label else "  " ++ label

-- Draw game code input field
drawGameCodeInput :: Maybe Text -> Widget MainMenuName
drawGameCodeInput Nothing = str "No code entered yet"
drawGameCodeInput (Just code) = str $ "Code: " ++ T.unpack code

-- Handle menu events
handleMainMenuEvent :: MainMenuState -> BrickEvent MainMenuName e -> EventM MainMenuName (Next MainMenuState)
-- Arrow keys
handleMainMenuEvent s (VtyEvent (V.EvKey V.KUp [])) =
  continue $ s { menuSelection = prevOption (menuSelection s) }
handleMainMenuEvent s (VtyEvent (V.EvKey V.KDown [])) =
  continue $ s { menuSelection = nextOption (menuSelection s) }

-- Enter key
handleMainMenuEvent s (VtyEvent (V.EvKey V.KEnter [])) =
  case menuSelection s of
    CreateGameOption -> suspendAndResume $ do
      -- Logic to create a new game
      return s
    JoinGameOption -> 
      case gameCode s of
        Nothing -> continue $ s { joinError = Just "Please enter a game code" }
        Just _ -> suspendAndResume $ do
          -- Logic to join a game
          return s
    LocalGameOption -> suspendAndResume $ do
      -- Logic to start a local game
      return s
    QuitOption -> halt s

-- Mouse clicks
handleMainMenuEvent s (MouseDown CreateButton _ _ _) =
  continue $ s { menuSelection = CreateGameOption }
handleMainMenuEvent s (MouseDown JoinButton _ _ _) =
  continue $ s { menuSelection = JoinGameOption }
handleMainMenuEvent s (MouseDown LocalButton _ _ _) =
  continue $ s { menuSelection = LocalGameOption }
handleMainMenuEvent s (MouseDown QuitButton _ _ _) =
  continue $ s { menuSelection = QuitOption }

-- Game code input (simplified)
handleMainMenuEvent s (VtyEvent (V.EvKey (V.KChar c) [])) =
  case menuSelection s of
    JoinGameOption -> 
      let newCode = maybe (T.singleton c) (`T.append` T.singleton c) (gameCode s)
      in continue $ s { gameCode = Just newCode, joinError = Nothing }
    _ -> continue s

-- Backspace to delete characters from game code
handleMainMenuEvent s (VtyEvent (V.EvKey V.KBS [])) =
  case menuSelection s of
    JoinGameOption ->
      let newCode = fmap (\t -> if T.null t then t else T.init t) (gameCode s)
      in continue $ s { gameCode = if maybe True T.null newCode then Nothing else newCode }
    _ -> continue s

-- Default
handleMainMenuEvent s _ = continue s

-- Get the next menu option
nextOption :: MainMenuSelection -> MainMenuSelection
nextOption CreateGameOption = JoinGameOption
nextOption JoinGameOption = LocalGameOption
nextOption LocalGameOption = QuitOption
nextOption QuitOption = CreateGameOption

-- Get the previous menu option
prevOption :: MainMenuSelection -> MainMenuSelection
prevOption CreateGameOption = QuitOption
prevOption JoinGameOption = CreateGameOption
prevOption LocalGameOption = JoinGameOption
prevOption QuitOption = LocalGameOption