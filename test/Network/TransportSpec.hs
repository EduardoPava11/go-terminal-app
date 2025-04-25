{-# LANGUAGE OverloadedStrings #-}
module Network.TransportSpec (spec) where

import Test.Hspec
import Network.Transport
import Network.Node (Node(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.Gossip.Plumtree as PT  -- Add this import
import qualified Network.Membership.HyParView as HV  -- Add this import

spec :: Spec
spec = do
  describe "Network Transport" $ do
    it "parses address correctly" $ do
      let (host, port) = parseAddr "127.0.0.1:9001"
      host `shouldBe` "127.0.0.1"
      port `shouldBe` "9001"
      
    it "encodes and decodes messages properly" $ do
      let testNode = Node "test" "localhost:9001"
          -- Create a test message with Plumtree payload
          ptMsg = PTMsg $ PT.RPC { PT.rpcPayload = "test-payload" }
          encoded = encodeGossipMessage ptMsg
          
      -- Check that encoding starts with correct message type byte (1 for Plumtree)
      BS.head encoded `shouldBe` 1
      
      -- Check that we can decode our own message
      case decodeGossipMessage encoded of
        Just (PTMsg rpc) -> PT.rpcPayload rpc `shouldBe` "test-payload"
        _ -> expectationFailure "Failed to decode message"
      
    it "encodes HyParView message correctly" $ do
      let hvMsg = HVMsg HV.RPC
          encoded = encodeGossipMessage hvMsg
      
      BS.head encoded `shouldBe` 0
      BS.length encoded `shouldBe` 1  -- Just the message type byte