{-# LANGUAGE OverloadedStrings #-}
module App.CLI
  ( CmdLine(..)
  , Mode(..)
  , parseCmdLine
  , prettyCmdLine
  ) where

import Options.Applicative

-- | Command-line mode
data Mode 
  = Listen Int           -- ^ Listen mode with port
  | Dial String Int      -- ^ Dial mode with host and port
  deriving (Show, Eq)

-- | Command-line options
data CmdLine = CmdLine 
  { mode    :: Mode      -- ^ Operation mode
  , boardSz :: Int       -- ^ Board size (9, 13, or 19)
  , uiKomi  :: Float     -- ^ Komi value
  } deriving (Show, Eq)

-- | Parse command-line arguments
parseCmdLine :: IO CmdLine
parseCmdLine = execParser opts
  where
    opts = info (cmdLineParser <**> helper)
      ( fullDesc
      <> progDesc "Play Go in the terminal"
      <> header "go-terminal-app - a terminal-based Go board game" )

-- | Command-line parser
cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
  <$> modeOption
  <*> sizeOption
  <*> komiOption

-- | Default port
defaultPort :: Int
defaultPort = 50555

-- | Parse mode option (combining listen and dial)
modeOption :: Parser Mode
modeOption = listenOption <|> dialOption <|> justPortOption <|> pure (Listen defaultPort)
  where
    -- Listen flag with no arguments (uses default port)
    listenOption = flag' (Listen defaultPort) 
                (long "listen" <> help "Listen for connections on default port")
    
    -- Port option that can be used with or without --listen
    portOption = option auto 
                (long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Port to listen on")
    
    -- Just the port option (implies listen mode)
    justPortOption = Listen <$> portOption
    
    -- Option for dial mode that parses HOST:PORT
    dialOption = option (eitherReader parseDialArg)
      ( long "dial"
        <> metavar "HOST:PORT" 
        <> help "Connect to a listening server at HOST:PORT" )

-- | Parse a dial argument in format host:port
parseDialArg :: String -> Either String Mode
parseDialArg s = case break (==':') s of
  (host, ':':portStr) -> 
    case reads portStr of
      [(port, "")] -> Right $ Dial host port
      _            -> Left $ "Invalid port number: " ++ portStr
  _ -> Left "Invalid format. Use HOST:PORT"

-- | Parse board size
sizeOption :: Parser Int
sizeOption = option auto
  ( long "size"
 <> short 's'
 <> metavar "SIZE"
 <> value 19
 <> help "Board size (9, 13, or 19)"
 <> showDefault )

-- | Parse komi value
komiOption :: Parser Float
komiOption = option auto
  ( long "komi"
 <> short 'k'
 <> metavar "FLOAT"
 <> value 7.5
 <> help "Komi value (compensation points for White)"
 <> showDefault )

-- | Pretty-print command line options for debugging
prettyCmdLine :: CmdLine -> String
prettyCmdLine cmd = 
  "Mode: " ++ showMode (mode cmd) ++ 
  ", Board size: " ++ show (boardSz cmd) ++ 
  ", Komi: " ++ show (uiKomi cmd)
  where
    showMode (Listen port) = "Listen (port: " ++ show port ++ ")"
    showMode (Dial host port) = "Dial " ++ host ++ ":" ++ show port