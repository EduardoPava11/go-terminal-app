{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Config
  ( NodeConfig(..)
  , NodeMode(..)
  , defaultConfig
  , loadConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs, lookupEnv)
import Data.Maybe (fromMaybe)

-- | Node operation modes
data NodeMode 
  = Gaming     -- ^ Active player node
  | Spectating -- ^ Read-only spectator node
  | Combined   -- ^ Both gaming and spectating
  deriving (Show, Eq, Generic)

instance ToJSON NodeMode
instance FromJSON NodeMode

-- | Configuration for network nodes
data NodeConfig = NodeConfig
  { -- Membership parameters
    maxActivePeers    :: Int     -- ^ Maximum number of active peers
  , maxPassivePeers   :: Int     -- ^ Maximum number of passive peers
  , shuffleInterval   :: Double  -- ^ Seconds between passive view shuffles
    -- Gossip parameters
  , gossipFanout      :: Int     -- ^ Number of peers to push each message to
  , gossipEagerRatio  :: Double  -- ^ Ratio of peers in eager set (0.0-1.0)
  , messageTimeout    :: Double  -- ^ Seconds to wait for message acknowledgment
    -- Node identity
  , nodeMode          :: NodeMode -- ^ Operating mode of this node
  , hostAddress       :: Text     -- ^ "host:port" for this node
  , nodeId            :: Maybe Text -- ^ Override for node ID (if not specified, a UUID is generated)
    -- General settings
  , verboseLogging    :: Bool     -- ^ Enable verbose logging
  } deriving (Show, Generic)

instance ToJSON NodeConfig
instance FromJSON NodeConfig

-- | Default configuration
defaultConfig :: NodeConfig
defaultConfig = NodeConfig
  { maxActivePeers    = 5
  , maxPassivePeers   = 30
  , shuffleInterval   = 30.0
  , gossipFanout      = 3
  , gossipEagerRatio  = 0.5
  , messageTimeout    = 2.0
  , nodeMode          = Gaming
  , hostAddress       = "127.0.0.1:9001"
  , nodeId            = Nothing
  , verboseLogging    = False
  }

-- | Load configuration from environment variables and command line
loadConfig :: IO NodeConfig
loadConfig = do
  -- Simply return the default config for now
  -- In a real implementation, you would parse environment variables, config files, etc.
  return defaultConfig