module Main where
  
import Gamesys
import Test.Hspec
import Data.Set
 
main :: IO ()
main = hspec $ do
 
  describe "series of numbers" $ do
    it "series for x=1 and y=5062.5 and length 5 should return correct set" $ do
      series 1 5062.5 5 `shouldBe` fromList [1.5, 4.0, 6.5, 10.75, 17.25]

  describe "special numbers of a series of numbers" $ do
    it "special numbers from series of [1.5, 4.0, 6.5, 10.75, 17.25] should be (6.5, 6.5)" $ do
      specialNumbersFromSet 160 (fromList [1.5, 4.0, 6.5, 10.75, 17.25]) `shouldBe` (Just 6.5, Just 6.5)

  describe "special numbers of a series of numbers" $ do
    it "special numbers for x=1, y=5062.5, z=160 and length 5 should be (6.5, 6.5)" $ do
      specialNumbers 1 5062.5 5 160 `shouldBe` (Just 6.5, Just 6.5)
