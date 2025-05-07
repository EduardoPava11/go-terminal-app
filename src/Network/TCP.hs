{-# LANGUAGE OverloadedStrings #-}
module Network.TCP
  ( runServer
  , connectPeer
  , sendMsg
  , recvMsg
  ) where

import qualified Network.Socket.ByteString.Lazy as NBL
import qualified Data.ByteString.Lazy as BL
import Network.SocketSimple
import Network.WireMsg

-- | Listen and hand the accepted socket to your handler in a new thread.
runServer :: HostPreference -> ServiceName -> (Socket -> IO ()) -> IO ()
runServer host port handler =
  withServer host port $ \(sock, _remote) -> handler sock

-- | Connect to host:port and give you the live socket.
connectPeer :: String -> String -> IO Socket
connectPeer h p = do
  var <- connectTo h p $ \(sock, _addr) -> return sock
  return var

------------------  one‑liners for the UI  ------------------

sendMsg :: Socket -> WireMsg -> IO ()
sendMsg sock = NBL.sendAll sock . encodeMsg

recvMsg :: Socket -> IO (Either String WireMsg)
recvMsg sock = do
  lbs <- NBL.recv sock 65536
  if BL.null lbs
     then pure $ Left "connection closed"
     else pure $ decodeMsg lbs