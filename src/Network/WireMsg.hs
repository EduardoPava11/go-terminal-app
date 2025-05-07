{-# LANGUAGE DeriveGeneric #-}

module Network.WireMsg
  ( WireMsg(..)
  , encodeMsg, decodeMsg
  , hashBoard
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson   (ToJSON, FromJSON, encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import           Game.Types   (Stone(..), GameHash, GameState)

-- | Every packet is self‑contained
data WireMsg
  = MovePlayed { who :: Stone, row :: Int, col :: Int, board :: GameHash }
  | PassPlayed { who :: Stone, board :: GameHash }
  | Resign     { who :: Stone }
  deriving (Show, Generic)

instance ToJSON   WireMsg
instance FromJSON WireMsg

encodeMsg  :: WireMsg -> BL.ByteString
encodeMsg  = encode

decodeMsg  :: BL.ByteString -> Either String WireMsg
decodeMsg  = eitherDecode

-- | Simple hash function for the game board
-- This is a placeholder - in a real implementation you'd calculate
-- a proper hash of the board state for the network protocol
hashBoard :: GameState -> GameHash
hashBoard _ = ""      -- placeholder - supplies required field