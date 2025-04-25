module UI.ThemeSpec (spec) where

import Test.Hspec
import UI.Theme

spec :: Spec
spec = do
  describe "UI Theme" $ do
    it "defines essential attribute names" $ do
      selectedAttr `shouldBe` attrName "selected"
      normalAttr `shouldBe` attrName "normal"
      cursorAttr `shouldBe` attrName "cursor"
    
    it "has a valid attribute map" $ do
      let attrs = attrMapAttrNames theMap
      length attrs `shouldSatisfy` (> 0)