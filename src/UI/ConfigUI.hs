{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module UI.ConfigUI
  ( runConfigForm
  , formStateToConfig
  ) where

import Brick
import qualified Brick.Main as B
import qualified Brick.Forms as F
import Brick.Widgets.Center as C
import Brick.Widgets.Border as B
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as CV
import qualified Brick.BChan as BCh
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Config (NodeConfig(..), NodeMode(..), defaultConfig)
import UI.Theme (theMap)
import Lens.Micro (lens, Lens')

-- Form field names
data FormFieldName 
  = HostField
  | PortField
  | ModeField
  | ActivPeersField
  | PassivPeersField
  | VerboseField
  deriving (Show, Eq, Ord)

-- Form state without lenses
data ConfigFormState = ConfigFormState
  { cfHost           :: Text
  , cfPort           :: Text
  , cfMode           :: NodeMode
  , cfMaxActivePeers :: Int
  , cfMaxPassivePeers :: Int
  , cfVerboseLogging :: Bool
  } deriving (Show)

-- Initialize form state from default config
initialFormState :: IO ConfigFormState
initialFormState = do
  let cfg = defaultConfig
      (host, port) = parseAddr (T.unpack $ hostAddress cfg)
  return $ ConfigFormState
    { cfHost = T.pack host
    , cfPort = T.pack port
    , cfMode = nodeMode cfg
    , cfMaxActivePeers = maxActivePeers cfg
    , cfMaxPassivePeers = maxPassivePeers cfg
    , cfVerboseLogging = verboseLogging cfg
    }

-- Build a form with explicit type annotations for compatibility
buildForm :: ConfigFormState -> F.Form ConfigFormState e FormFieldName
buildForm = F.newForm fields
  where
    fields = 
      [ F.editTextField hostLens HostField (Just 1)
      , F.editTextField portLens PortField (Just 1)  
      , F.radioField modeLens
          [(Gaming, ModeField, "Gaming")
          ,(Spectating, ModeField, "Spectating")
          ,(Combined, ModeField, "Combined")
          ]
      , F.editShowableField activePeersLens ActivPeersField
      , F.editShowableField passivePeersLens PassivPeersField
      , F.checkboxField verboseLogLens VerboseField "Enable verbose logging"
      ]
    
    -- Explicitly typed lenses for compatibility
    hostLens :: Lens' ConfigFormState Text
    hostLens = lens cfHost (\s t -> s { cfHost = t })
    
    portLens :: Lens' ConfigFormState Text
    portLens = lens cfPort (\s t -> s { cfPort = t })
    
    modeLens :: Lens' ConfigFormState NodeMode
    modeLens = lens cfMode (\s v -> s { cfMode = v })
    
    activePeersLens :: Lens' ConfigFormState Int
    activePeersLens = lens cfMaxActivePeers (\s v -> s { cfMaxActivePeers = v })
    
    passivePeersLens :: Lens' ConfigFormState Int
    passivePeersLens = lens cfMaxPassivePeers (\s v -> s { cfMaxPassivePeers = v })
    
    verboseLogLens :: Lens' ConfigFormState Bool
    verboseLogLens = lens cfVerboseLogging (\s v -> s { cfVerboseLogging = v })

-- Validate form inputs
validateForm :: F.Form ConfigFormState e FormFieldName -> (Bool, [String])
validateForm form =
  let state = F.formState form
      portValid = case reads (T.unpack $ cfPort state) :: [(Int, String)] of
                    [(p, "")] -> p > 0 && p < 65536
                    _         -> False
      errors = concat
        [ if T.null (cfHost state) then ["Host cannot be empty"] else []
        , if not portValid then ["Port must be a number between 1-65535"] else []
        ]
  in (null errors, errors)

-- Draw the form with nice borders
drawForm :: F.Form ConfigFormState e FormFieldName -> [Widget FormFieldName]
drawForm form =
  [ C.center $ B.borderWithLabel (str "Network Configuration") $ 
    padAll 2 $ 
    vBox [ F.renderForm form
         , B.hBorder
         , hBox [ padLeft (Pad 2) $ str "Press Enter to save, Esc to cancel" ]
         ]
  ]

-- Handle form events with the right type signature for Brick 2.3+
formEventHandler :: BrickEvent FormFieldName e 
                 -> EventM FormFieldName (F.Form ConfigFormState e FormFieldName) ()
formEventHandler (VtyEvent (V.EvKey V.KEsc [])) = 
  liftIO (putStrLn "Form canceled")
formEventHandler (VtyEvent (V.EvKey V.KEnter [])) = do
  form <- get
  case validateForm form of
    (True, _)  -> liftIO (putStrLn "Form submitted")
    (False, _) -> return ()
formEventHandler ev = F.handleFormEvent ev

-- Convert the form state to a NodeConfig
formStateToConfig :: ConfigFormState -> NodeConfig
formStateToConfig state =
  let hostPort = T.unpack (cfHost state) <> ":" <> T.unpack (cfPort state)
  in defaultConfig
    { hostAddress = T.pack hostPort
    , nodeMode = cfMode state
    , maxActivePeers = cfMaxActivePeers state
    , maxPassivePeers = cfMaxPassivePeers state
    , verboseLogging = cfVerboseLogging state
    }

-- Run the config form and return the resulting NodeConfig
runConfigForm :: IO NodeConfig
runConfigForm = do
  initialState <- initialFormState
  
  -- Create a new BChan for events
  chan <- BCh.newBChan 10
  
  -- Create Vty using proper initialization for Vty 6.x
  vty <- CV.mkVty V.defaultConfig
  
  let buildVty = CV.mkVty V.defaultConfig
  
  -- Define app with proper Brick 2.3+ handler pattern
  let app = configApp chan
              
  finalState <- B.customMain vty buildVty (Just chan) app (buildForm initialState)
  return $ formStateToConfig (F.formState finalState)

-- Define app with proper Brick 2.3+ handler pattern
configApp :: BCh.BChan () -> App (F.Form ConfigFormState () FormFieldName) () FormFieldName
configApp chan = App 
  { appDraw = drawForm
  , appChooseCursor = showFirstCursor
  , appHandleEvent = formEventHandler
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

-- | Parse an address string in format "host:port" into a tuple (host, port)
parseAddr :: String -> (String, String)
parseAddr addr =
  case break (==':') addr of
    (host, ':':port) -> (host, port)
    (host, _)        -> (host, "9001")  -- Default port