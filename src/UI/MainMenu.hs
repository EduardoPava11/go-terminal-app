{-# LANGUAGE OverloadedStrings #-}

module UI.MainMenu
  ( drawMainMenu
  , handleMainMenuEvent
  , MainMenuState(..)
  , MainMenuSelection(..)
  , MenuMode(..)
  ) where

import Brick
import Brick.Types (BrickEvent(..), EventM)
import Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Main as BM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import UI.Theme (selectedAttr, normalAttr)

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
  , menuMode :: MenuMode    -- Track if exiting
  }

data MenuMode = Normal | Exiting
  deriving (Eq, Show)

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
  , menuMode = Normal
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

-- Handle menu events with event-first signature for Brick 2.3+
handleMainMenuEvent :: BrickEvent MainMenuName e -> MainMenuState -> EventM MainMenuName e MainMenuState
-- Arrow key navigation
handleMainMenuEvent (VtyEvent (V.EvKey V.KUp [])) s =
  return $ s { menuSelection = prevOption (menuSelection s) }
handleMainMenuEvent (VtyEvent (V.EvKey V.KDown [])) s =
  return $ s { menuSelection = nextOption (menuSelection s) }

-- Enter key handler for menu options
handleMainMenuEvent (VtyEvent (V.EvKey V.KEnter [])) s =
  case menuSelection s of
    CreateGameOption -> 
      return s
    
    JoinGameOption -> 
      case gameCode s of
        Nothing -> return $ s { joinError = Just "Please enter a game code" }
        Just _ -> 
          return s
    
    LocalGameOption -> 
      return s
      
    QuitOption -> 
      return $ s { menuMode = Exiting }

-- Mouse clicks
handleMainMenuEvent (MouseDown CreateButton _ _ _) s =
  return $ s { menuSelection = CreateGameOption }
handleMainMenuEvent (MouseDown JoinButton _ _ _) s =
  return $ s { menuSelection = JoinGameOption }
handleMainMenuEvent (MouseDown LocalButton _ _ _) s =
  return $ s { menuSelection = LocalGameOption }
handleMainMenuEvent (MouseDown QuitButton _ _ _) s =
  return $ s { menuSelection = QuitOption }

-- Game code input (simplified)
handleMainMenuEvent (VtyEvent (V.EvKey (V.KChar c) [])) s =
  case menuSelection s of
    JoinGameOption -> 
      let newCode = maybe (T.singleton c) (`T.append` T.singleton c) (gameCode s)
      in return $ s { gameCode = Just newCode, joinError = Nothing }
    _ -> return s

-- Backspace to delete characters from game code
handleMainMenuEvent (VtyEvent (V.EvKey V.KBS [])) s =
  case menuSelection s of
    JoinGameOption ->
      let newCode = fmap (\t -> if T.null t then t else T.init t) (gameCode s)
      in return $ s { gameCode = if maybe True T.null newCode then Nothing else newCode }
    _ -> return s

-- Default
handleMainMenuEvent _ s = return s

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