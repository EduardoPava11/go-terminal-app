{-# LANGUAGE OverloadedStrings #-}
module Network.ConfigSpec (spec) where

import Test.Hspec
import Network.Config

spec :: Spec
spec = do
  describe "Network Configuration" $ do
    it "has gaming mode by default" $ do
      let config = defaultConfig
      nodeMode config `shouldBe` Gaming
      
    it "has correct default network parameters" $ do
      let config = defaultConfig
      maxActivePeers config `shouldBe` 5
      maxPassivePeers config `shouldBe` 30
      gossipFanout config `shouldBe` 3
      
    it "has localhost as default address" $ do
      config <- loadConfig
      hostAddress config `shouldBe` "127.0.0.1:9001"