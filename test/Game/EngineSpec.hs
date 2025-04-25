module Game.EngineSpec (spec) where

import Test.Hspec
import Game.Engine
import Game.Types
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "Game Engine" $ do
    it "initializes with Black to play" $ do
      let state = initialGameState 9
      currentPlayer state `shouldBe` Black
      
    it "advances move number on valid moves" $ do
      let state = initialGameState 9
          Right newState = applyMove Pass state
      moveNumber newState `shouldBe` 2
      
    it "alternates between players" $ do
      let state = initialGameState 9
          Right state2 = applyMove Pass state
          Right state3 = applyMove Pass state2
      currentPlayer state `shouldBe` Black
      currentPlayer state2 `shouldBe` White
      currentPlayer state3 `shouldBe` Black
      
    it "rejects out of bounds moves" $ do
      let state = initialGameState 9
          result = applyMove (Place 100 100) state
      result `shouldSatisfy` isLeft
      
    it "rejects moves on occupied spots" $ do
      let state = initialGameState 9
          Right state2 = applyMove (Place 0 0) state
          result = applyMove (Place 0 0) state2
      result `shouldSatisfy` isLeft
      
    it "correctly places stones on the board" $ do
      let state = initialGameState 9
          Right state2 = applyMove (Place 4 4) state  -- Place stone in center
          boardVec = board state2
          centerIdx = 4 * 9 + 4
          
      (boardVec V.! centerIdx) `shouldBe` Black
      
    it "handles resignation properly" $ do
      let state = initialGameState 9
          result = applyMove Resign state
      
      result `shouldSatisfy` isLeft
      case result of
        Left msg -> msg `shouldBe` "Player resigned"
        _ -> expectationFailure "Expected resignation"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False