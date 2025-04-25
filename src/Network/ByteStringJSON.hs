{-# LANGUAGE OverloadedStrings #-}

module Network.ByteStringJSON () where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)

-- JSON instances for ByteString (using Base64 encoding)
instance ToJSON BS.ByteString where
  toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON BS.ByteString where
  parseJSON = withText "ByteString" $ \t ->
    either (const $ fail "Invalid Base64") return $ B64.decode (encodeUtf8 t)