{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Gossip.Plumtree
  ( MessageId(..)
  , RPC(..)
  , Plumtree(..)
  , Env(..)  -- This already exports all field accessors
  , ApplyResult(..)
  , PlumtreeCont(..)
  , new
  , runPlumtree
  , receive
  , broadcast
  , neighborUp
  , neighborDown
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Control.Monad (ap, when)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar', readTVarIO)

-- Import the Network.Config module
import Network.Config (NodeConfig(..), NodeMode(..))

-- Message identifier
newtype MessageId = MessageId Text
  deriving (Eq, Show, Ord)  -- Added Ord instance here

-- Remote procedure call
data RPC n = RPC 
  { rpcPayload :: ByteString 
  } deriving (Show)

-- Environment type
data Env n = Env
  { envConfig :: NodeConfig
  , envNode :: n
  , eagerPeers :: TVar (Set n)
  , lazyPeers :: TVar (Set n)
  , messagesSeen :: TVar (Map MessageId ByteString)
  , pendingMessages :: TVar (Map MessageId (TVar Int))
  }

-- Custom Show instance for Env
instance Show n => Show (Env n) where
  show env = "Env { envConfig = " ++ show (envConfig env) ++ 
             ", envNode = " ++ show (envNode env) ++
             ", eagerPeers = <TVar>," ++
             " lazyPeers = <TVar>," ++
             " messagesSeen = <TVar>," ++
             " pendingMessages = <TVar> }"

-- Result of applying a message
data ApplyResult = Applied (Maybe MessageId) | Stale (Maybe MessageId)
  deriving (Eq, Show)

-- Free monad-style definition of Plumtree
data Plumtree n a
  = Pure a
  | ReceiveF (RPC n) (Plumtree n a)
  | BroadcastF MessageId ByteString (Plumtree n a)
  | NeighborUpF n (Plumtree n a)
  | NeighborDownF n (Plumtree n a)
  deriving (Functor, Show)

-- Make Plumtree a proper monad
instance Applicative (Plumtree n) where
  pure = Pure
  (<*>) = ap

instance Monad (Plumtree n) where
  return = pure
  Pure a >>= f = f a
  ReceiveF rpc next >>= f = ReceiveF rpc (next >>= f)
  BroadcastF mid bs next >>= f = BroadcastF mid bs (next >>= f)
  NeighborUpF n next >>= f = NeighborUpF n (next >>= f)
  NeighborDownF n next >>= f = NeighborDownF n (next >>= f)

-- Constructor for environment
new :: NodeConfig -> n -> IO (Env n)
new config node = do
  eager <- newTVarIO S.empty
  lazy <- newTVarIO S.empty
  seen <- newTVarIO M.empty
  pending <- newTVarIO M.empty
  return $ Env
    { envConfig = config
    , envNode = node
    , eagerPeers = eager
    , lazyPeers = lazy
    , messagesSeen = seen
    , pendingMessages = pending
    }

-- Receive an RPC
receive :: RPC n -> Plumtree n ()
receive rpc = ReceiveF rpc (Pure ())

-- Broadcast a message
broadcast :: MessageId -> ByteString -> Plumtree n ()
broadcast mid bs = BroadcastF mid bs (Pure ())

-- Add a neighbor to the eager set
neighborUp :: n -> Plumtree n ()
neighborUp n = NeighborUpF n (Pure ())

-- Remove a neighbor from the eager set
neighborDown :: n -> Plumtree n ()
neighborDown n = NeighborDownF n (Pure ())

-- Data constructors for Plumtree continuations
data PlumtreeCont n a
  = ApplyMessage MessageId ByteString (ApplyResult -> Plumtree n a)
  | LookupMessage MessageId (Maybe ByteString -> Plumtree n a)
  | SendEager n (RPC n) (Plumtree n a)
  | SendLazy n Int [MessageId] (Plumtree n a)
  | Later Double MessageId (Plumtree n a) (Plumtree n a)
  | Cancel MessageId (Plumtree n a)
  | Done a

-- Manual Show instance for PlumtreeCont
instance (Show n, Show a) => Show (PlumtreeCont n a) where
  show (ApplyMessage mid bs _) = 
    "ApplyMessage " ++ show mid ++ " " ++ show bs ++ " <function>"
  show (LookupMessage mid _) = 
    "LookupMessage " ++ show mid ++ " <function>"
  show (SendEager n rpc _) = 
    "SendEager " ++ show n ++ " " ++ show rpc ++ " <plumtree>"
  show (SendLazy n round mids _) = 
    "SendLazy " ++ show n ++ " " ++ show round ++ " " ++ show mids ++ " <plumtree>"
  show (Later timeout mid _ _) = 
    "Later " ++ show timeout ++ " " ++ show mid ++ " <plumtree> <plumtree>"
  show (Cancel mid _) = 
    "Cancel " ++ show mid ++ " <plumtree>"
  show (Done a) = 
    "Done " ++ show a

-- Runner for Plumtree actions
-- Add Show n constraint to fix the second error
runPlumtree :: (Show n, Ord n) => Env n -> Plumtree n a -> IO (PlumtreeCont n a)
runPlumtree env (Pure a) = return (Done a)
runPlumtree env (ReceiveF rpc next) = do
  -- Here we would process the received message
  putStrLn "[Plumtree] Received message"
  -- For now, simulate creating an ApplyMessage continuation
  let mid = MessageId "received-message-id"
      payload = rpcPayload rpc
  return (ApplyMessage mid payload (\_ -> next))
runPlumtree env (BroadcastF mid bs next) = do
  let config = envConfig env
      mode = nodeMode config
  
  when (verboseLogging config) $
    putStrLn $ "[Plumtree] Broadcasting message " ++ show mid

  -- Store the message as seen
  atomically $ modifyTVar' (messagesSeen env) (M.insert mid bs)
  
  -- Get eager peers to push to
  eagerSet <- readTVarIO (eagerPeers env)

  -- Adjust fanout based on node mode
  let baseFanout = gossipFanout config
      adjustedFanout = case mode of
                        Gaming -> baseFanout  -- Full fanout for gaming nodes
                        Spectating -> max 1 (baseFanout `div` 2)  -- Lower fanout for spectators
                        Combined -> baseFanout
  
  -- If no eager peers, just return Done with the value in next
  if S.null eagerSet
    then do
      -- Extract the value from next using a helper function
      let extractValue (Pure a) = a
          extractValue _ = error "Expected Pure constructor"
      return (Done (extractValue next))
    else do
      -- Choose random eager peers based on fanout
      let selected = take adjustedFanout (S.toList eagerSet)
      
      -- If we have a peer, return SendEager for the first one
      case selected of
        (peer:_) -> do
          -- Create a counter to track pending acks
          counter <- newTVarIO (length selected - 1)
          atomically $ modifyTVar' (pendingMessages env) (M.insert mid counter)
          
          -- Schedule a timeout based on config
          let timeout = messageTimeout config
          
          return $ SendEager peer (RPC bs) next
        [] -> do
          -- Similar to the S.null eagerSet case
          let extractValue (Pure a) = a
              extractValue _ = error "Expected Pure constructor"
          return (Done (extractValue next))
runPlumtree env (NeighborUpF n next) = do
  putStrLn $ "[Plumtree] Adding neighbor: " ++ show n
  -- Extract the value from next
  let extractValue (Pure a) = a
      extractValue _ = error "Expected Pure constructor"
  return (Done (extractValue next))
runPlumtree env (NeighborDownF n next) = do
  putStrLn $ "[Plumtree] Removing neighbor: " ++ show n
  -- Extract the value from next
  let extractValue (Pure a) = a
      extractValue _ = error "Expected Pure constructor"
  return (Done (extractValue next))