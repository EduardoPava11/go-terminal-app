{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.ConfigUI
  ( runConfigForm
  , ConfigFormState(..)
  ) where

import Brick
import qualified Brick.Forms as BF
import Brick.Widgets.Center as C
import Brick.Widgets.Border as B
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Brick.Focus (focusRing, focusRingCursor)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)

import Data.Text (Text)
import qualified Data.Text as T
import Network.Config (NodeConfig(..), NodeMode(..), defaultConfig)
import UI.Theme (theMap, selectedAttr, normalAttr)
import System.Exit (exitSuccess)
import Network.Socket (getAddrInfo, defaultHints, AddrInfo(..), AddrInfoFlag(..))
import Network.Info (getNetworkInterfaces, NetworkInterface(..), IPv4(..))
import Data.List (intercalate)

-- Form field names
data FormFieldName
  = PortField
  | HostField
  | NodeIdField
  | ModeField
  | ActivePeersField
  | PassivePeersField
  | FanoutField
  | VerboseField
  deriving (Show, Eq, Ord)

-- Form state with lenses
data ConfigFormState = ConfigFormState
  { _cfgPort :: Int
  , _cfgHost :: Text
  , _cfgNodeId :: Text
  , _cfgMode :: NodeMode
  , _cfgActivePeers :: Int
  , _cfgPassivePeers :: Int
  , _cfgFanout :: Int
  , _cfgVerbose :: Bool
  } deriving (Show, Eq)

makeLenses ''ConfigFormState

-- Initialize form state from default config
initialFormState :: IO ConfigFormState
initialFormState = do
  let cfg = defaultConfig
      port = read $ T.unpack $ last $ T.splitOn ":" (hostAddress cfg)
  
  -- Try to get the local IP address
  localIPs <- getLocalIPAddresses
  let hostname = case localIPs of
                   [] -> T.pack "127.0.0.1"
                   (ip:_) -> T.pack ip
  
  return $ ConfigFormState
    { _cfgPort = port
    , _cfgHost = hostname
    , _cfgNodeId = T.pack "player1"
    , _cfgMode = nodeMode cfg
    , _cfgActivePeers = maxActivePeers cfg
    , _cfgPassivePeers = maxPassivePeers cfg
    , _cfgFanout = gossipFanout cfg
    , _cfgVerbose = verboseLogging cfg
    }

-- Fixed version that works with Brick 0.70
buildForm :: ConfigFormState -> BF.Form ConfigFormState e FormFieldName
buildForm = BF.newForm
  [ BF.editTextField cfgHost HostField (Just 1)
  , BF.editShowableField cfgPort PortField
  , BF.editTextField cfgNodeId NodeIdField (Just 1)
  , BF.radioField cfgMode 
      [(Gaming, ModeField, "Gaming (active player)")
      ,(Spectating, ModeField, "Spectating (observer only)")
      ,(Combined, ModeField, "Combined (both roles)")
      ]
  , BF.editShowableField cfgActivePeers ActivePeersField
  , BF.editShowableField cfgPassivePeers PassivePeersField
  , BF.editShowableField cfgFanout FanoutField
  , BF.checkboxField cfgVerbose VerboseField "Enable"
  ]

-- Validate form inputs
validateForm :: BF.Form ConfigFormState e FormFieldName -> (Bool, [String])
validateForm form =
  let state = BF.formState form
      errors = concat
        [ if state^.cfgPort <= 0 || state^.cfgPort > 65535
            then ["Port must be between 1 and 65535"]
            else []
        , if T.null (state^.cfgHost)
            then ["Host cannot be empty"]
            else []
        , if T.null (state^.cfgNodeId)
            then ["Node ID cannot be empty"]
            else []
        , if state^.cfgActivePeers < 1
            then ["Active peers must be at least 1"]
            else []
        , if state^.cfgPassivePeers < state^.cfgActivePeers
            then ["Passive peers should not be less than active peers"]
            else []
        , if state^.cfgFanout < 1
            then ["Fanout must be at least 1"]
            else []
        ]
  in (null errors, errors)

-- Custom form rendering with labels
-- Draw the form with nice borders and sections for Brick 0.70
drawForm :: BF.Form ConfigFormState e FormFieldName -> [Widget FormFieldName]
drawForm form =
  let (isValid, errors) = validateForm form
      -- We'll use BF.renderForm which handles everything internally
      formWidget = addFieldLabels $ BF.renderForm form

      errorWidget = if isValid
                    then emptyWidget
                    else B.borderWithLabel (withAttr selectedAttr $ str "Validation Errors") $
                         vBox $ map (withAttr selectedAttr . str . ("• " ++)) errors
  in
  [ C.center $
      padAll 1 $
        vBox
          [ B.borderWithLabel (str "Terminal Go - Network Configuration") formWidget
          , padTop (Pad 1) errorWidget
          , padTop (Pad 1) $
              withAttr selectedAttr $
                str "Press [Enter] to confirm, [Esc] to quit"
          ]
  ]

-- Helper function to add field labels
-- In Brick 0.70 we need to handle labels differently
addFieldLabels :: Widget FormFieldName -> Widget FormFieldName
addFieldLabels w = Widget Fixed Fixed $ do
  ctx <- getContext
  let fieldLabels = vBox 
        [ withAttr normalAttr $ str "Host:"
        , withAttr normalAttr $ str "Port:"
        , withAttr normalAttr $ str "Node ID:"
        , withAttr normalAttr $ str "Node Mode:"
        , withAttr normalAttr $ str "Max Active Peers:"
        , withAttr normalAttr $ str "Max Passive Peers:"
        , withAttr normalAttr $ str "Gossip Fanout:"
        , withAttr normalAttr $ str "Verbose Logging:"
        ]
  render $ hBox [padRight (Pad 2) fieldLabels, w]

-- Handle form events with correct signature
handleConfigFormEvent :: (MonadIO m) => BF.Form ConfigFormState e FormFieldName -> 
                         BrickEvent FormFieldName e -> 
                         EventM FormFieldName (m (BF.Form ConfigFormState e FormFieldName))
handleConfigFormEvent form ev = 
  case ev of
    VtyEvent (V.EvKey V.KEsc []) -> liftIO exitSuccess >> continue form
    VtyEvent (V.EvKey V.KEnter []) ->
      let (isValid, _) = validateForm form
      in if isValid then halt form else continue form
    _ -> do
        newForm <- BF.handleFormEvent ev form
        continue newForm

-- Convert the form state to a NodeConfig
formStateToConfig :: ConfigFormState -> NodeConfig
formStateToConfig state = NodeConfig
  { maxActivePeers = state^.cfgActivePeers
  , maxPassivePeers = state^.cfgPassivePeers
  , shuffleInterval = 30.0  -- Default
  , gossipFanout = state^.cfgFanout
  , gossipEagerRatio = 0.5  -- Default
  , messageTimeout = 2.0    -- Default
  , nodeMode = state^.cfgMode
  , hostAddress = T.concat [state^.cfgHost, ":", T.pack (show (state^.cfgPort))]
  , nodeId = Just (state^.cfgNodeId)
  , verboseLogging = state^.cfgVerbose
  }

-- Run the config form and return the resulting NodeConfig
runConfigForm :: IO NodeConfig
runConfigForm = do
  initialState <- initialFormState
  let buildAttrMap = const $ attrMap V.defAttr
        [ (BF.formAttr, V.white `on` V.blue)
        , (BF.invalidFormInputAttr, V.white `on` V.red)
        , (BF.focusedFormInputAttr, V.black `on` V.yellow)
        , (selectedAttr, V.withStyle (V.white `on` V.black) V.bold)
        , (normalAttr, V.white `on` V.black)
        ]
      
      formFocus = focusRing
        [ HostField
        , PortField
        , NodeIdField
        , ModeField
        , ActivePeersField
        , PassivePeersField
        , FanoutField
        , VerboseField
        ]
      
      app = App
        { appDraw = drawForm
        , appHandleEvent = handleConfigFormEvent
        , appChooseCursor = focusRingCursor (\_ -> formFocus)
        , appStartEvent = pure
        , appAttrMap = buildAttrMap
        }
        
  let initialForm = buildForm initialState
  
  -- Create Vty instance for customMain
  vty <- V.mkVty V.defaultConfig
  finalForm <- defaultMain app initialForm
  
  -- Get local IPs and display them
  ips <- getLocalIPAddresses
  let config = formStateToConfig (BF.formState finalForm)
      hostPort = T.unpack (hostAddress config)
  
  putStrLn "Configuration accepted!"
  putStrLn $ "Your node is configured at: " ++ hostPort
  
  when (length ips > 1) $ do
    putStrLn "Local IP addresses detected:"
    mapM_ (\ip -> putStrLn $ "  " ++ ip) ips
    putStrLn $ "Others can connect to you using: [yourIP]:" ++ (T.unpack $ last $ T.splitOn ":" (hostAddress config))
  
  return config

-- Helper function to get local IP addresses
getLocalIPAddresses :: IO [String]
getLocalIPAddresses = do
  interfaces <- getNetworkInterfaces
  let ips = [show ip | iface <- interfaces, let IPv4 ip = ipv4 iface, ip /= 0]
  return ips