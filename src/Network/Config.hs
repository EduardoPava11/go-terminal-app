{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Config
  ( -- * Configuration Types
    NodeConfig(..)
  , NodeMode(..)
    -- * Default and Utility Functions
  , defaultConfig
  , loadConfig
  , validateConfig
  , configToString
  , defaultPort
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import System.Environment (lookupEnv)

-- | Default port for network connections
defaultPort :: Int
defaultPort = 50555

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
  { -- | Membership parameters
    maxActivePeers    :: Int     -- ^ Maximum number of active peers (direct connections)
  , maxPassivePeers   :: Int     -- ^ Maximum number of passive peers (potential connections)
  , shuffleInterval   :: Double  -- ^ Seconds between passive view shuffles for network health
    -- | Gossip parameters
  , gossipFanout      :: Int     -- ^ Number of peers to push each message to initially
  , gossipEagerRatio  :: Double  -- ^ Ratio of peers in eager set (0.0-1.0)
  , messageTimeout    :: Double  -- ^ Seconds to wait for message acknowledgment
    -- | Node identity
  , nodeMode          :: NodeMode -- ^ Operating mode of this node
  , hostAddress       :: Text     -- ^ "host:port" for this node
  , nodeId            :: Maybe Text -- ^ Override for node ID (if not specified, a UUID is generated)
    -- | General settings
  , verboseLogging    :: Bool     -- ^ Enable verbose logging
  } deriving (Show, Generic)

instance ToJSON NodeConfig
instance FromJSON NodeConfig

-- | Default configuration with reasonable values for a local development setup
defaultConfig :: NodeConfig
defaultConfig = NodeConfig
  { maxActivePeers    = 5      -- Maintain 5 active connections
  , maxPassivePeers   = 30     -- Keep 30 peers in our passive view
  , shuffleInterval   = 30.0   -- Shuffle passive peers every 30 seconds
  , gossipFanout      = 3      -- Send new messages to 3 peers initially
  , gossipEagerRatio  = 0.5    -- Keep 50% of connected peers in eager mode
  , messageTimeout    = 2.0    -- Wait 2 seconds for acknowledgment
  , nodeMode          = Gaming -- Default to gaming mode
  , hostAddress       = "127.0.0.1:9001"  -- Default local address
  , nodeId            = Nothing -- Generate a random UUID by default
  , verboseLogging    = False   -- Verbose logging off by default
  }

-- | Load configuration from environment variables and command line
loadConfig :: IO NodeConfig
loadConfig = do
  -- Get environment variable overrides with fallbacks to default values
  maxActive <- readEnvInt "MAX_ACTIVE_PEERS" (maxActivePeers defaultConfig)
  maxPassive <- readEnvInt "MAX_PASSIVE_PEERS" (maxPassivePeers defaultConfig)
  shuffle <- readEnvDouble "SHUFFLE_INTERVAL" (shuffleInterval defaultConfig)
  fanout <- readEnvInt "GOSSIP_FANOUT" (gossipFanout defaultConfig)
  eagerRatio <- readEnvDouble "GOSSIP_EAGER_RATIO" (gossipEagerRatio defaultConfig)
  timeout <- readEnvDouble "MESSAGE_TIMEOUT" (messageTimeout defaultConfig)
  verbose <- readEnvBool "VERBOSE_LOGGING" (verboseLogging defaultConfig)
  
  -- Get host address from environment or use default
  hostAddr <- lookupEnv "HOST_ADDRESS"
  let hostAddrText = maybe (hostAddress defaultConfig) T.pack hostAddr
  
  -- Get node ID from environment
  nodeIdEnv <- lookupEnv "NODE_ID"
  let nodeIdText = fmap T.pack nodeIdEnv
  
  -- Get node mode from environment
  modeEnv <- lookupEnv "NODE_MODE"
  let mode = case modeEnv of
              Just "gaming"     -> Gaming
              Just "spectating" -> Spectating
              Just "combined"   -> Combined
              _                 -> nodeMode defaultConfig
  
  -- Build and validate the configuration
  let config = NodeConfig
        { maxActivePeers    = maxActive
        , maxPassivePeers   = maxPassive
        , shuffleInterval   = shuffle
        , gossipFanout      = fanout 
        , gossipEagerRatio  = eagerRatio
        , messageTimeout    = timeout
        , nodeMode          = mode
        , hostAddress       = hostAddrText
        , nodeId            = nodeIdText
        , verboseLogging    = verbose
        }
      
  -- Return validated config or default if validation fails
  return $ either (const defaultConfig) id (validateConfig config)

-- | Helper function to read an Int from an environment variable
readEnvInt :: String -> Int -> IO Int
readEnvInt name defaultVal = do
  mVal <- lookupEnv name
  return $ case mVal of
    Just str -> either (const defaultVal) id (readEither str)
    Nothing  -> defaultVal

-- | Helper function to read a Double from an environment variable  
readEnvDouble :: String -> Double -> IO Double
readEnvDouble name defaultVal = do
  mVal <- lookupEnv name
  return $ case mVal of
    Just str -> either (const defaultVal) id (readEither str)
    Nothing  -> defaultVal

-- | Helper function to read a Bool from an environment variable
readEnvBool :: String -> Bool -> IO Bool
readEnvBool name defaultVal = do
  mVal <- lookupEnv name
  return $ case mVal of
    Just "true"  -> True
    Just "True"  -> True
    Just "1"     -> True
    Just "false" -> False
    Just "False" -> False
    Just "0"     -> False
    _            -> defaultVal

-- | Parse a string as either an Int or Double
readEither :: Read a => String -> Either String a
readEither s =
  case reads s of
    [(val, "")] -> Right val
    _           -> Left $ "Cannot parse " ++ s

-- | Validate configuration values and return Either error message or validated config
validateConfig :: NodeConfig -> Either String NodeConfig
validateConfig config
  | maxActivePeers config <= 0 =
      Left "maxActivePeers must be positive"
  | maxPassivePeers config < 0 =
      Left "maxPassivePeers cannot be negative"
  | shuffleInterval config <= 0 =
      Left "shuffleInterval must be positive"
  | gossipFanout config <= 0 =
      Left "gossipFanout must be positive"
  | gossipEagerRatio config < 0 || gossipEagerRatio config > 1 =
      Left "gossipEagerRatio must be between 0.0 and 1.0"
  | messageTimeout config <= 0 =
      Left "messageTimeout must be positive"
  | T.null (hostAddress config) =
      Left "hostAddress cannot be empty"
  | otherwise =
      Right config

-- | Convert a configuration to a human-readable string
configToString :: NodeConfig -> String
configToString config =
  "Node Configuration:\n" ++
  "- maxActivePeers: " ++ show (maxActivePeers config) ++ "\n" ++
  "- maxPassivePeers: " ++ show (maxPassivePeers config) ++ "\n" ++
  "- shuffleInterval: " ++ show (shuffleInterval config) ++ " seconds\n" ++
  "- gossipFanout: " ++ show (gossipFanout config) ++ "\n" ++
  "- gossipEagerRatio: " ++ show (gossipEagerRatio config * 100) ++ "%\n" ++
  "- messageTimeout: " ++ show (messageTimeout config) ++ " seconds\n" ++
  "- nodeMode: " ++ show (nodeMode config) ++ "\n" ++
  "- hostAddress: " ++ T.unpack (hostAddress config) ++ "\n" ++
  "- verboseLogging: " ++ (if verboseLogging config then "ON" else "OFF")