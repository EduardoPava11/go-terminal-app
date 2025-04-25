{-# LANGUAGE OverloadedStrings #-}
module Network.Transport (newTransport) where

import Network.Transport (Transport)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- | Create a TCP transport that listens on the supplied (host,port).
newTransport :: String -> String -> IO Transport
newTransport host port = do
  Right t <- createTransport host port defaultTCPParameters
  pure t