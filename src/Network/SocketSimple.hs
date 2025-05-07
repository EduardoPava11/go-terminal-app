{-# LANGUAGE OverloadedStrings #-}
module Network.SocketSimple 
  ( sendBS
  , recvBS
  , withServer
  , connectTo
  , Socket
  , HostPreference
  , ServiceName
  , SockAddr
  , connectSend
  , startListener
  , PortNumber
  ) where

import qualified Network.Socket as N
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString      as BS
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever)
import Control.Exception (catch, throw, SomeException)

-- | Socket type alias
type Socket = N.Socket

-- | Host preference type
type HostPreference = String

-- | Service name (port)
type ServiceName = String

-- | Socket address alias
type SockAddr = N.SockAddr

-- | Port number type
type PortNumber = String

-- | Run a TCP server
withServer :: HostPreference -> ServiceName -> ((Socket, SockAddr) -> IO ()) -> IO ()
withServer host port handler = do
  addr <- resolve host port
  N.withSocketsDo $ bracket (createServerSocket addr) N.close $ \sock -> do
    N.setSocketOption sock N.ReuseAddr 1
    N.bind sock (N.addrAddress addr)
    N.listen sock 5
    forever $ do
      (conn, peer) <- N.accept sock
      handler (conn, peer)
  where
    createServerSocket addr = N.socket (N.addrFamily addr) N.Stream N.defaultProtocol

-- Helper to resolve address
resolve :: HostPreference -> ServiceName -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  head <$> N.getAddrInfo (Just hints) (Just host) (Just port)

-- Helper for bracket pattern
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release action = do
  resource <- acquire
  result <- action resource `onException` release resource
  _ <- release resource  -- Fixed: Added _ <- to bind the result
  return result

-- | Exception handler
onException :: IO a -> IO b -> IO a
onException act cleanup = act `catch` (\e -> cleanup >> throw (e :: SomeException))

-- | Connect to a remote server
connectTo :: N.HostName -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
connectTo host port action = do
  addr <- resolve host port
  N.withSocketsDo $ bracket (createClientSocket addr) N.close $ \sock -> do
    N.connect sock (N.addrAddress addr)
    action (sock, N.addrAddress addr)
  where
    createClientSocket addr = N.socket (N.addrFamily addr) N.Stream N.defaultProtocol

-- | Send a ByteString over a socket
sendBS :: Socket -> BS.ByteString -> IO ()
sendBS = sendAll

-- | Receive a ByteString from a socket
recvBS :: Socket -> IO (Maybe BS.ByteString)
recvBS s = do
  bs <- recv s 4096
  return $ if BS.null bs then Nothing else Just bs

-- | Start a listener on the given port and handle connections with the provided function
startListener :: PortNumber -> ((Socket, SockAddr) -> IO ()) -> IO ThreadId
startListener port handler = forkIO $ withServer "0.0.0.0" port handler

-- | Connect to a host, send a message, then disconnect
connectSend :: N.HostName -> PortNumber -> BS.ByteString -> IO ()
connectSend host port message = 
  connectTo host port $ \(sock, _) -> do
    sendBS sock message