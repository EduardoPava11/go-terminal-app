{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Network.Messages 
  ( GameMessage(..)
  , encodeGameMessage
  , decodeGameMessage
  , HPMessage(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.Aeson as A
import Control.Distributed.Process
import Data.Binary (Binary)

import Game.Types (Move, Stone) 
import Network.Node (Node)
import Network.ByteStringJSON ()  -- Import ByteString JSON instances

-- | Messages passed between nodes in the network
data GameMessage
  = MsgCreateGame { gmGameId :: Text, gmBlack :: Node, gmWhite :: Maybe Node }
  | MsgJoinGame   { gmGameId :: Text, gmSeat  :: Stone, gmPlayer :: Node }
  | MsgMove       { gmGameId :: Text, gmMove  :: Move }
  | MsgSyncRequest { gmGameId :: Text }
  | MsgSyncResponse { gmGameId :: Text, gmGameState :: Text }  -- Using Text instead of ByteString
  deriving (Show, Generic)

instance ToJSON GameMessage
instance FromJSON GameMessage

-- | Encode a game message to ByteString for network transmission
encodeGameMessage :: GameMessage -> ByteString
encodeGameMessage = BL.toStrict . encode

-- | Decode a game message from ByteString
decodeGameMessage :: ByteString -> Maybe GameMessage
decodeGameMessage = decode . BL.fromStrict

--------------------------------------------------------------------------------
-- HyParView RPCs
--------------------------------------------------------------------------------

data HPMessage
    = Join   ProcessId                -- a node wants to join
    | ForwardJoin [ProcessId] Int     -- relay join (ttl)
    | NeighborReq ProcessId Bool      -- active = True | passive = False
    | ShuffleReq ProcessId [ProcessId] Int
    | ShuffleResp [ProcessId]
    deriving (Generic, Binary)