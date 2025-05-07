{-# LANGUAGE OverloadedStrings #-}

module UI.Event (handleEvent) where

import Brick (BrickEvent(..), EventM)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Data.IORef

import Game.Types
import Game.Engine hiding (Resign)
import qualified Network.WireMsg as WM
import Network.WireMsg (hashBoard)
import Network.TCP (sendMsg)
import Network.SocketSimple (Socket)

-- | Handle UI events - updated signature for Brick 2.3+
handleEvent :: BrickEvent Name () -> Socket -> IORef GameState -> UIState -> EventM Name () UIState
handleEvent ev sock gsRef us = case ev of
  -- Mouse click events
  MouseDown (UIBoardCell r c) _ _ _ -> 
    let sel = SelectCoord r c
    in handleSelection sock gsRef us sel
  
  MouseDown PassButton _ _ _ ->
    handleSelection sock gsRef us SelectPass
    
  MouseDown ResignButton _ _ _ ->
    handleSelection sock gsRef us SelectResign
  
  -- Keyboard navigation
  VtyEvent (V.EvKey V.KUp []) -> do
    return $ moveCursor us (-1, 0)
    
  VtyEvent (V.EvKey V.KDown []) -> do
    return $ moveCursor us (1, 0)
    
  VtyEvent (V.EvKey V.KLeft []) -> do
    return $ moveCursor us (0, -1)
    
  VtyEvent (V.EvKey V.KRight []) -> do
    return $ moveCursor us (0, 1)
  
  -- Enter to select
  VtyEvent (V.EvKey V.KEnter []) ->
    let r = fst (cursor us)
        c = snd (cursor us)
        sel = SelectCoord r c
    in handleSelection sock gsRef us sel
  
  -- Space to toggle between Pass and Resign
  VtyEvent (V.EvKey (V.KChar ' ') []) -> do
    return $ us { selection = toggleSelection (selection us) }
  
  -- Escape to clear selection
  VtyEvent (V.EvKey V.KEsc []) -> do
    return $ us { selection = NoSelection }
  
  -- Quit with 'q'
  VtyEvent (V.EvKey (V.KChar 'q') []) -> do
    liftIO $ putStrLn "Quitting game"
    return us
  
  -- Default case
  _ -> return us

-- Handle a selection and send network message if needed
handleSelection :: Socket -> IORef GameState -> UIState -> UISelection -> EventM Name () UIState
handleSelection sock gsRef us sel = 
  case sel of
    SelectCoord r c -> 
      let gs = gameState us
      in case makeMove gs (r, c) of
          Left _ -> do 
            return us  -- invalid move
          
          Right newGs -> do
            -- Send the move over the network
            liftIO $ do
              -- Calculate game hash using provided helper
              let gameHash = hashBoard newGs
              sendMsg sock (WM.MovePlayed (currentPlayer gs) r c gameHash)
              writeIORef gsRef newGs
            return $ us { gameState = newGs, selection = NoSelection }
            
    SelectPass ->
      let gs = gameState us
          newGs = passTurn gs
      in do
        -- Send pass message over network
        liftIO $ do
          let gameHash = hashBoard newGs
          sendMsg sock (WM.PassPlayed (currentPlayer gs) gameHash)
          writeIORef gsRef newGs
        return $ us { gameState = newGs, selection = NoSelection }
        
    SelectResign ->
      do
        -- Send resign message over network
        liftIO $ sendMsg sock (WM.Resign (currentPlayer (gameState us)))
        -- We're not updating the game state for resign since it's handled by the receiver
        return $ us { selection = NoSelection }
        
    NoSelection ->
      return $ us { selection = sel }

-- Helper for toggling between Pass and Resign selections
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