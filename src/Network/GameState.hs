{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.GameState
  ( NetworkGame(..)
  , createGame
  , joinGame
  , applyNetworkMove
  , getCurrentGameId
  , byteStringToText
  , textToByteString
  , syncGameState
  , handlePlayerJoin
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import qualified Data.ByteString.Base64 as B64
import GHC.Generics (Generic)

import Game.Types
import Game.Engine
import Network.Node (Node)
import Network.Messages (GameMessage(..))
import Network.Gossip.Plumtree (Env, MessageId(..), broadcast, envConfig)
import Network.Config (NodeConfig, NodeMode(..), nodeMode, verboseLogging)
import qualified Network.Gossip.Plumtree as PT

-- | Represents a network game state
data NetworkGame = NetworkGame
  { ngGameId :: Text        -- ^ Unique game identifier
  , ngBlack :: Node         -- ^ Black player node
  , ngWhite :: Maybe Node   -- ^ White player node (Nothing if waiting for player)
  , ngGameState :: GameState -- ^ Current game state
  , ngLocalPlayer :: Stone  -- ^ Which color the local player is
  } deriving (Show, Generic)

instance ToJSON NetworkGame
instance FromJSON NetworkGame

-- | Check if actions are allowed based on mode
canPerformGameAction :: NodeConfig -> IO Bool
canPerformGameAction config =
  case nodeMode config of
    Spectating -> do
      putStrLn "[GameState] Cannot perform game action in spectator mode"
      return False
    _ -> return True

-- | Create a new network game as black
createGame :: Env Node -> Node -> IO NetworkGame
createGame ptEnv localNode = do
  let config = envConfig ptEnv
  
  canAct <- canPerformGameAction config
  if not canAct
    then error "Cannot create game in spectator mode"
    else do
      -- Generate a unique game ID
      randomUUID <- UUID.nextRandom
      let gameId = T.pack $ show randomUUID
      
      -- Create initial game state (9x9 board)
      let gs = initialGameState 9
          ng = NetworkGame 
                 { ngGameId = gameId
                 , ngBlack = localNode
                 , ngWhite = Nothing
                 , ngGameState = gs
                 , ngLocalPlayer = Black
                 }
      
      -- Broadcast game creation message
      let createMsg = MsgCreateGame 
                        { gmGameId = gameId
                        , gmBlack = localNode
                        , gmWhite = Nothing
                        }
      
      -- Generate a unique message ID for Plumtree
      randomMsgUUID <- UUID.nextRandom
      let msgId = MessageId (T.pack $ show randomMsgUUID)
          encodedMsg = BL.toStrict $ encode createMsg
      
      -- Broadcast using Plumtree
      _ <- PT.runPlumtree ptEnv $ broadcast msgId encodedMsg
      
      putStrLn $ "[Network] Created new game: " ++ T.unpack gameId
      return ng

-- | Join an existing game
joinGame :: Env Node -> Text -> Stone -> Node -> IO NetworkGame
joinGame ptEnv gameId seat localNode = do
  let config = envConfig ptEnv
  
  canAct <- canPerformGameAction config
  if not canAct
    then error "Cannot join game in spectator mode"
    else do
      -- Create initial game state (9x9 board)
      let gs = initialGameState 9
          ng = NetworkGame
                 { ngGameId = gameId
                 , ngBlack = if seat == Black then localNode else error "Expected black player info"
                 , ngWhite = if seat == White then Just localNode else Nothing
                 , ngGameState = gs
                 , ngLocalPlayer = seat
                 }
      
      -- Broadcast join message
      let joinMsg = MsgJoinGame
                      { gmGameId = gameId
                      , gmSeat = seat
                      , gmPlayer = localNode
                      }
      
      -- Generate a unique message ID for Plumtree
      randomMsgUUID <- UUID.nextRandom
      let msgId = MessageId (T.pack $ show randomMsgUUID)
          encodedMsg = BL.toStrict $ encode joinMsg
      
      -- Broadcast using Plumtree
      _ <- PT.runPlumtree ptEnv $ broadcast msgId encodedMsg
      
      putStrLn $ "[Network] Joined game: " ++ T.unpack gameId
      return ng

-- | Apply a move from the network
applyNetworkMove :: Env Node -> NetworkGame -> Move -> IO (Either String NetworkGame)
applyNetworkMove ptEnv ng move = do
  let config = envConfig ptEnv
  
  canAct <- canPerformGameAction config
  if not canAct
    then return $ Left "Cannot apply move in spectator mode"
    else do
      -- Check if it's the local player's turn
      let gs = ngGameState ng
          currentPlayerColor = currentPlayer gs
      
      if currentPlayerColor == ngLocalPlayer ng
        then do
          -- Apply the move locally
          case applyMove move gs of
            Left err -> 
              return $ Left err
            Right newGs -> do
              -- Broadcast the move
              let moveMsg = MsgMove
                               { gmGameId = ngGameId ng
                               , gmMove = move
                               }
              
              -- Generate a unique message ID for Plumtree
              randomMsgUUID <- UUID.nextRandom
              let msgId = MessageId (T.pack $ show randomMsgUUID)
                  encodedMsg = BL.toStrict $ encode moveMsg
              
              -- Broadcast using Plumtree
              _ <- PT.runPlumtree ptEnv $ broadcast msgId encodedMsg
              
              -- Update local state
              let updatedNg = ng { ngGameState = newGs }
              return $ Right updatedNg
        else
          return $ Left "Not your turn"

-- | Sync the game state with the network
syncGameState :: Env Node -> NetworkGame -> IO ()
syncGameState ptEnv ng = do
  -- Serialize the game state to ByteString
  let gs = ngGameState ng
      gsBS = BL.toStrict $ encode gs
      
  -- Convert to Base64 Text for JSON serialization
  let gsText = byteStringToText gsBS
      
  -- Create sync message
  let syncMsg = MsgSyncResponse
                  { gmGameId = ngGameId ng
                  , gmGameState = gsText  -- Use Text here
                  }
                  
  -- Generate message ID and broadcast
  randomMsgUUID <- UUID.nextRandom
  let msgId = MessageId (T.pack $ show randomMsgUUID)
      encodedMsg = BL.toStrict $ encode syncMsg
      
  _ <- PT.runPlumtree ptEnv $ broadcast msgId encodedMsg
  putStrLn "[Network] Synced game state"

-- | Handle a player joining the game
handlePlayerJoin :: NetworkGame -> GameMessage -> IO NetworkGame
handlePlayerJoin ng (MsgJoinGame gameId seat player)
  | gameId /= ngGameId ng = do
      putStrLn $ "[Network] Ignoring join for different game: " ++ T.unpack gameId
      return ng
  | otherwise = do
      putStrLn $ "[Network] Player joined as " ++ show seat ++ ": " ++ show player
      case seat of
        White ->
          return ng { ngWhite = Just player }
        Black ->
          return ng { ngBlack = player }
        _ ->
          return ng
handlePlayerJoin ng _ = return ng

-- | Get the current game ID
getCurrentGameId :: NetworkGame -> Text
getCurrentGameId = ngGameId

-- | Converts ByteString to Base64-encoded Text
byteStringToText :: ByteString -> Text
byteStringToText bs = TE.decodeUtf8 (B64.encode bs)

-- | Converts Base64-encoded Text back to ByteString
textToByteString :: Text -> Maybe ByteString
textToByteString t = either (const Nothing) Just $ B64.decode (TE.encodeUtf8 t)