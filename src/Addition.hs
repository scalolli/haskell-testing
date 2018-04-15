module Addition where

  import Test.Hspec
  import Test.QuickCheck

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


  integralMultiplication :: Integral a => a -> a -> a
  integralMultiplication x 0 = 0
  integralMultiplication 0 y = 0
  integralMultiplication x y = go x x y
      where go a b count
              | count == 1 = a
              | otherwise = go (a + b) b (count-1)


  trivialInt :: Gen Int
  trivialInt = return 1


  genBool :: Gen Bool
  genBool = choose (True, False)

  genBool' :: Gen Bool
  genBool' = elements [True, False]

  genOrdering :: Gen Ordering
  genOrdering = elements [LT, EQ, GT]

  genChar :: Gen Char
  genChar = elements ['a'..'z']

  genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
  genTuple = do
      a <- arbitrary
      b <- arbitrary
      return (a, b)


  genMaybe :: Arbitrary a => Gen (Maybe a)
  genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]


  genMaybe' :: Arbitrary a => Gen (Maybe a)
  genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing), (9, return (Just a))]

  main :: IO ()
  main = hspec $ do
    describe "Addition" $ do
      it "15 divided by 3 is 5" $ do
        dividedBy 15 3 `shouldBe` (5, 0)

      it "22 divided by 5 is\
        \ 4 remainder 2" $ do
          dividedBy 22 5 `shouldBe` (4, 2)

    describe "Integral Multiplication" $ do
      it "2 multiplied 3 is 6" $ do
        integralMultiplication 2 3 `shouldBe` 6

      it "2 multiplied 0 is 0" $ do
        integralMultiplication 2 0 `shouldBe` 0

      it "5 mutiplied 1 is 2" $ do
        integralMultiplication 2 1 `shouldBe` 2

    describe "Property testing example" $ do
      it "successive number should be greater than 1 from its previous number" $ do
        property $ \x -> x + 1 > (x :: Int)


