{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Network.Membership
  ( initHyParView
  , startHyParViewThread
  , hyParViewLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Network.Node (Node)
import Network.Membership.HyParView
import Network.Config (NodeConfig)

-- | Initialize HyParView membership protocol
initHyParView :: NodeConfig -> Node -> IO (Env Node)
initHyParView config node = do
  env <- new config node
  return env

-- | Start the HyParView background thread
startHyParViewThread :: Env Node -> [Node] -> IO ()
startHyParViewThread env contacts = do
  putStrLn $ "[Membership] Starting HyParView thread with " ++ show (length contacts) ++ " contacts"
  
  -- Join the network if we have contacts
  void $ runHyParView env $ do
    mapM_ joinInitial contacts
    shufflePassive
    
  -- Start the background thread
  void $ forkIO $ hyParViewLoop env (return ())
  putStrLn $ "[Membership] HyParView thread started"

-- | Main loop for HyParView - MVP stub implementation
hyParViewLoop :: (Show Node, Ord Node) => Env Node -> HyParView Node () -> IO ()
hyParViewLoop env action = do
  -- MVP implementation - just log and return
  putStrLn "[Membership] HyParView processing (MVP stub)"
  return ()