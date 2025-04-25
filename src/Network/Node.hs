{-# LANGUAGE TemplateHaskell #-}
module Network.Node (withNode, Node(..)) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import qualified Network.Transport                     as NT
import qualified Network.Membership.HyParView as HP
import           Network.Messages             ( HPMessage )
import           Control.Monad                ( forM_, void )
import           Data.Text                    ( Text )

-- Define Node type for using in the application
data Node = Node
  { nodeId   :: Text
  , nodeAddr :: Text
  } deriving (Show, Eq, Ord)

------------------------------------------------------------------------------
-- 1.  Remote-table boiler-plate (Template Haskell)
------------------------------------------------------------------------------

remoteTable :: RemoteTable
remoteTable = __remoteTable initRemoteTable

------------------------------------------------------------------------------
-- 2.  Helper that spins up a LocalNode and runs the supplied Process
------------------------------------------------------------------------------

withNode
  :: String                 -- ^ host (e.g. "0.0.0.0")
  -> String                 -- ^ port (e.g. "10501")
  -> [ProcessId]            -- ^ seed nodes (can be empty)
  -> (LocalNode -> Process ())   -- ^ your node program
  -> IO ()
withNode host port seeds app = do
  t  <- NT.newTransport host port
  ln <- newLocalNode t remoteTable
  runProcess ln $ do
    -- Spawn HyParView first; pass the seed list
    _hp <- spawnLocal (HP.runHyParView seeds)
    -- If we have seeds, send JOIN
    forM_ seeds HP.joinNetwork
    link _hp           -- die if membership process dies ⇒ easier debugging
    app ln             -- continue to user program