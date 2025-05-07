{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.TCP (runServer, connectPeer, recvMsg)
import Network.SocketSimple (Socket)
import qualified Network.WireMsg as WM
import Game.Types
import qualified Game.Engine as Engine    
import UI.UI (uiLoop)
import Data.IORef
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Exception (try, SomeException)
import App.CLI
import Network.Config (defaultPort)
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  cmdLine <- parseCmdLine
  let boardSize = boardSz cmdLine
      komiValue = uiKomi cmdLine

  printf "Go Terminal App with %s\n" (prettyCmdLine cmdLine)
  
  case mode cmdLine of
    Listen port -> do
      -- In Listen mode, start a server and wait for a connection
      printf "Waiting for connections on port %d...\n" port
      runServer "0.0.0.0" (show port) $ \sock -> do
        printf "Peer connected! You play as %s\n" (show White)
        let initialGs = Engine.initialGameState boardSize
            updatedGs = initialGs { gameConfig = GameConfig boardSize komiValue }
        gsRef <- newIORef updatedGs
        
        _ <- forkIO (rxLoop sock gsRef)
        uiLoop sock gsRef
    
    Dial host port -> do
      -- In Dial mode, connect to the remote host
      printf "Connecting to %s:%d...\n" host port
      dialResult <- try (connectPeer host (show port))
      case dialResult of
        Left (e :: SomeException) -> do
          printf "Connection failed: %s\n" (show e)
          exitFailure
        Right sock -> do
          printf "Connected to %s:%d! You play as %s\n" host port (show Black)
          let initialGs = Engine.initialGameState boardSize
              updatedGs = initialGs { gameConfig = GameConfig boardSize komiValue }
          gsRef <- newIORef updatedGs
          
          _ <- forkIO (rxLoop sock gsRef)
          uiLoop sock gsRef

-- | Continuously receive and process messages from the socket
rxLoop :: Socket -> IORef GameState -> IO ()
rxLoop sock ref = forever $ do
  result <- recvMsg sock
  case result of
    Left err -> putStrLn $ "Network error: " ++ err
    Right msg -> modifyIORef' ref (applyNet msg)
  where
    applyNet (WM.MovePlayed _ r c _) gs = 
      case Engine.makeMove gs (r, c) of
        Left _ -> gs -- Keep original state on error
        Right newGs -> newGs
    applyNet (WM.PassPlayed _ _) gs = Engine.passTurn gs
    applyNet (WM.Resign _) gs = gs -- Just keep original state for now