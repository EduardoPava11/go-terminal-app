{-# LANGUAGE OverloadedStrings #-}

module UI.UI (uiLoop) where

import Brick
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as CV
import qualified Brick.Main as M
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Network.SocketSimple (Socket)
import Game.Engine hiding (Resign)
import qualified Network.WireMsg as WM
import Network.WireMsg (hashBoard)
import Network.TCP (sendMsg)

import Game.Types
import UI.Board
import UI.Buttons
import UI.Overlay
import UI.Theme

-- | The main UI loop - starts Brick with our app
uiLoop :: Socket -> IORef GameState -> IO ()
uiLoop sock gsRef = do
  -- Initial setup
  initialGs <- readIORef gsRef
  let initialState = UIState
        { gameState = initialGs
        , selection = NoSelection
        , cursor = (0, 0)  -- Start at top-left
        , networkStatus = Disconnected
        , connectionInfo = Nothing
        , peers = mempty
        , opponent = Nothing
        , errorMessage = Nothing
        }
  
  -- Start the UI with updated Vty initialization for Vty 6.x
  vty <- CV.mkVty V.defaultConfig
  
  -- Use customMain with the proper Vty 6 initialization
  let buildVty = liftIO $ CV.mkVty V.defaultConfig
  _ <- M.customMain vty buildVty Nothing (app sock gsRef) initialState
  
  -- Clean up on exit
  return ()

-- | The Brick App definition with event-first pattern for handlers
app :: Socket -> IORef GameState -> App UIState () Name
app sock gsRef = App
  { appDraw         = drawUI gsRef
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = UI.UI.handleEvent sock gsRef
  , appStartEvent   = return ()
  , appAttrMap      = const theMap
  }

-- | Handle events with proper type signature for Brick 2.3
handleEvent :: Socket -> IORef GameState -> BrickEvent Name () -> EventM Name UIState ()
handleEvent sock gsRef ev = do
  st <- get
  
  -- Handle the event directly rather than trying to call UI.Event.handleEvent
  case ev of
    -- Mouse click events
    MouseDown (UIBoardCell r c) _ _ _ -> 
      handleCoordSelection sock gsRef st r c
    
    MouseDown PassButton _ _ _ ->
      handlePassSelection sock gsRef st
    
    MouseDown ResignButton _ _ _ ->
      handleResignSelection sock gsRef st
    
    -- Keyboard navigation
    VtyEvent (V.EvKey V.KUp []) -> do
      put $ moveCursor st (-1, 0)
    
    VtyEvent (V.EvKey V.KDown []) -> do
      put $ moveCursor st (1, 0)
    
    VtyEvent (V.EvKey V.KLeft []) -> do
      put $ moveCursor st (0, -1)
    
    VtyEvent (V.EvKey V.KRight []) -> do
      put $ moveCursor st (0, 1)
    
    -- Enter to select
    VtyEvent (V.EvKey V.KEnter []) -> do
      let r = fst (cursor st)
          c = snd (cursor st)
      handleCoordSelection sock gsRef st r c
    
    -- Space to toggle between Pass and Resign
    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      put $ st { selection = toggleSelection (selection st) }
    
    -- Escape to clear selection
    VtyEvent (V.EvKey V.KEsc []) -> do
      put $ st { selection = NoSelection }
      halt
    
    -- Quit with 'q' or Ctrl+q
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      liftIO $ putStrLn "Quitting game"
      halt
      
    VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]) -> halt
    
    -- Default case
    _ -> return ()

-- Helper functions replicating UI.Event functionality
handleCoordSelection :: Socket -> IORef GameState -> UIState -> Int -> Int -> EventM Name UIState ()
handleCoordSelection sock gsRef us r c = do
  let gs = gameState us
  case makeMove gs (r, c) of
    Left _ -> return ()  -- invalid move
    Right newGs -> do
      -- Send the move over the network
      liftIO $ do
        let gameHash = hashBoard newGs
        sendMsg sock (WM.MovePlayed (currentPlayer gs) r c gameHash)
        writeIORef gsRef newGs
      put $ us { gameState = newGs, selection = NoSelection }

handlePassSelection :: Socket -> IORef GameState -> UIState -> EventM Name UIState ()
handlePassSelection sock gsRef us = do
  let gs = gameState us
      newGs = passTurn gs
  -- Send pass message over network
  liftIO $ do
    let gameHash = hashBoard newGs
    sendMsg sock (WM.PassPlayed (currentPlayer gs) gameHash)
    writeIORef gsRef newGs
  put $ us { gameState = newGs, selection = NoSelection }

handleResignSelection :: Socket -> IORef GameState -> UIState -> EventM Name UIState ()
handleResignSelection sock gsRef us = do
  -- Send resign message over network
  liftIO $ sendMsg sock (WM.Resign (currentPlayer (gameState us)))
  put $ us { selection = NoSelection }

-- Helpers replicated from UI.Event
toggleSelection :: UISelection -> UISelection
toggleSelection SelectPass = SelectResign
toggleSelection SelectResign = SelectPass
toggleSelection other = other

-- | Move the cursor with wrapping
moveCursor :: UIState -> (Int, Int) -> UIState
moveCursor s (dr, dc) =
  let (r, c) = cursor s
      bs = boardSize (gameState s)
      r' = (r + dr) `mod` bs
      c' = (c + dc) `mod` bs
  in s { cursor = (r', c') }

-- | Draw the UI
drawUI :: IORef GameState -> UIState -> [Widget Name]
drawUI gsRef s = [drawUI' gsRef s]
  
-- | Helper function to draw UI that reads from IORef
drawUI' :: IORef GameState -> UIState -> Widget Name
drawUI' gsRef s = do
  let getLatestState = do
        currentGs <- readIORef gsRef
        let updatedState = s { gameState = currentGs }
        return updatedState
      
  -- Build the widget with the current state
  vBox
    [ hBox [ vBox [ drawBoardWithCoords (gameState s) (selection s) (cursor s)
                  , drawButtons (selection s)
                  ]
           ]
    , drawStatus s
    ]