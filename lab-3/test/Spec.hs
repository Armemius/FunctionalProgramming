import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True