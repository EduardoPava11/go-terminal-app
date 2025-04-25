module UI.BoardSpec (spec) where

import Test.Hspec
import UI.Board
import Game.Types  -- Make sure all tests use the updated type names (Stone vs Player, etc.)
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "UI Board" $ do
    it "renders stone correctly" $ do
      drawStone Empty `shouldBe` "."
      drawStone Black `shouldBe` "●"
      drawStone White `shouldBe` "○"