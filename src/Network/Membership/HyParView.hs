{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Network.Membership.HyParView
  ( runHyParView          -- main process
  , joinNetwork           -- bootstrap
  , viewActive, viewPassive
  , stateFromPid          -- helper for Main.hs
  ) where

import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable ()
import           Control.Exception                        ( SomeException )
import           Control.Monad                            ( forM_, void
                                                          , when, forever )
import           Data.Set                                 ( Set )
import qualified Data.Set                                as S
import           Network.Messages                         ( HPMessage(..) )

-- | Parameter limits (tune later).
aMax, pMax :: Int
aMax = 4            -- active view size
pMax = 12           -- passive view size

-------------------------------------------------------------------------------
-- Internal mutable state wrapped in STM TVars
-------------------------------------------------------------------------------
data HPState = HPState
  { activeView  :: TVar (Set ProcessId)
  , passiveView :: TVar (Set ProcessId)
  }

newState :: IO HPState
newState = atomically $ HPState <$> newTVar S.empty <*> newTVar S.empty

-------------------------------------------------------------------------------
-- Public helpers
-------------------------------------------------------------------------------
viewActive, viewPassive :: HPState -> STM (Set ProcessId)
viewActive  st = readTVar (activeView  st)
viewPassive st = readTVar (passiveView st)

-- | Helper to get the state from a PID (for Main.hs)
stateFromPid :: Maybe ProcessId -> Process HPState
stateFromPid _ = liftIO newState  -- Simplified for now; in a real implementation
                                  -- you'd use whereis or process registry

-- | Top-level loop; spawn this once and forget.
runHyParView :: [ProcessId] -> Process ()
runHyParView seeds = do
  self   <- getSelfPid
  state  <- liftIO newState
  -- Bootstrap: populate passive view with the seeds we know (minus self)
  liftIO . atomically $ modifyTVar' (passiveView state)
        (S.union (S.fromList (filter (/= self) seeds)))

  -- Start background shuffle timer (every 30 s)
  void . spawnLocal $ shuffleTimer state

  -- Main receive loop
  forever $
    receiveWait
      [ match $ handleHP state
      , match $ \(ProcessMonitorNotification _ dead _) ->
          liftIO . atomically $ do
            modifyTVar' (activeView  state) (S.delete dead)
            modifyTVar' (passiveView state) (S.insert dead) -- maybe revive later
      ]
  where
    handleHP st = \case
      Join newcomer -> do
        self <- getSelfPid
        -- Accept newcomer: reply with NeighborReq True
        send newcomer (NeighborReq self True)
        promoteActive st newcomer
      ForwardJoin path 0 ->   -- TTL expired: store in passive only
        addPassive st path
      ForwardJoin path ttl -> do
        self <- getSelfPid
        myAct <- liftIO . atomically $ viewActive st
        -- relay to a random active peer (except the sender)
        let next = head (filter (`notElem` path) (S.toList myAct) ++ [head path])
        send next (ForwardJoin (self:path) (ttl-1))
      NeighborReq pid wantActive -> do
        if wantActive
           then promoteActive st pid
           else addPassive  st [pid]
      ShuffleReq origin sample ttl -> do
        addPassive st sample
        when (ttl == 0) $ do
          sample' <- liftIO (samplePassive st)
          send origin (ShuffleResp sample')
        when (ttl > 0) $ do
          self <- getSelfPid
          myAct <- liftIO . atomically $ viewActive st
          let next = head (filter (/= origin) (S.toList myAct) ++ [origin])
          send next (ShuffleReq origin sample (ttl-1))
      ShuffleResp peers ->
        addPassive st peers

-- Promote a peer into the active view (drop oldest if we overflow).
promoteActive :: HPState -> ProcessId -> Process ()
promoteActive st pid = do
  -- Monitor the peer to detect failures
  void $ monitor pid
  -- Update the views
  liftIO . atomically $ do
    modifyTVar' (passiveView st) (S.delete pid)
    modifyTVar' (activeView  st) (\s -> takeMax aMax pid s)
  where
    takeMax n x s | S.size s < n = S.insert x s
                  | otherwise    = let (old,rest) = S.deleteFindMin s
                                   in S.insert x rest

-- Insert peers into passive view up to limit.
addPassive :: HPState -> [ProcessId] -> Process ()
addPassive st ps = liftIO . atomically $
  modifyTVar' (passiveView st) (\s -> foldr (takeMax pMax) s ps)
  where
    takeMax n x s | S.size s < n = S.insert x s
                  | otherwise    = let (old,rest) = S.deleteFindMin s
                                   in S.insert x rest

-- Random sample of k passive peers for shuffle (k = 3).
samplePassive :: HPState -> IO [ProcessId]
samplePassive st = atomically (readTVar (passiveView st))
               >>= pure . take 3 . S.toList  -- simple for now

-- Every 30 s pick a random active peer and shuffle
shuffleTimer :: HPState -> Process ()
shuffleTimer st = forever $ do
  liftIO (threadDelay 30_000_000)
  myPid <- getSelfPid
  act   <- liftIO . atomically $ viewActive st
  case S.toList act of
    []      -> pure ()  -- alone for now
    (p:_)   -> do
      sample <- liftIO (samplePassive st)
      -- ttl = 2 to avoid long routes in small nets
      send p (ShuffleReq myPid sample 2)

-------------------------------------------------------------------------------
--  joinNetwork  ---------------------------------------------------------------
-------------------------------------------------------------------------------
-- | Called once at startup if you know at least one existing node
joinNetwork :: ProcessId -> Process ()
joinNetwork seed = do
  self <- getSelfPid
  -- Send a JOIN directly; seed node will handle forward-join recursion
  send seed (Join self)

  -- Also monitor the seed so we get failure notifications
  void $ monitor seed