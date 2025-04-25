{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad          (forever, void)
import Control.Concurrent     (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode, localNodeId)
import qualified Network.Node as N
import qualified Network.Membership.HyParView as HP
import qualified Data.Set as S

------------------------------------------------------------------------
-- 1.  Very small node program used just for Week-2 testing
------------------------------------------------------------------------
nodeProgram :: LocalNode -> Process ()
nodeProgram ln = do
  self <- getSelfPid
  liftIO $ putStrLn $ "Node up. PID = " ++ show self
  
  -- Wait 3 s to give HyParView time to fill activeView
  liftIO (threadDelay 3_000_000)
  say "Attempting to pick first active peer…"
  hpPid <- expectTimeout 0 :: Process (Maybe ProcessId)  -- not used yet

  -- Use a synchronous STM read (simple for demo)
  hpState <- HP.stateFromPid hpPid
  act <- liftIO . atomically $ HP.viewActive hpState
  case S.toList act of
    []      -> say "No peers yet – waiting…"
    (p:_)   -> pingPeer p
    
  -- Keep node alive
  forever $ liftIO $ threadDelay 10_000_000

pingPeer :: ProcessId -> Process ()
pingPeer peer = do
  say $ "Pinging " ++ show peer
  send peer ("ping" :: String)
  receiveWait
    [ matchIf (== ("pong" :: String)) $ \_ ->
        say "Received pong – success!" ]

------------------------------------------------------------------------
-- 2.  Entry point
------------------------------------------------------------------------
main :: IO ()
main = do
  -- pass different ports when you run the second machine
  N.withNode "0.0.0.0" "10501" [] nodeProgram