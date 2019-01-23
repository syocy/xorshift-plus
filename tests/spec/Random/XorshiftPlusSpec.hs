{-# LANGUAGE TypeApplications #-}

module Random.XorshiftPlusSpec where

import Test.Hspec
import Random.XorshiftPlus
import Data.List

calcChisq :: [Int] -> Double -> Double
calcChisq count expected = foldl' f 0.0 $ count
  where
    f :: Double -> Int -> Double
    f r x = r + ((((fromIntegral x) - expected)) ** 2 / expected)

testChisq :: Int -> Int -> [Int] -> Double
testChisq n k randoms = calcChisq count (fromIntegral n / fromIntegral k)
  where
    minInt = toInteger (minBound :: Int)
    maxInt = toInteger (maxBound :: Int)
    step = (maxInt - minInt) `div` (toInteger k)
    randoms' = map (\r -> fromInteger @Int ((toInteger r - minInt) `div` step)) randoms
    count = map length . group $ sort randoms'

execTestChisq :: IO Bool
execTestChisq = do
  xorshift <- genXorshiftPlusInt 1
  let (k, limit) = (10, 16.919)
  let n = k * 100
  randoms <- sequence $ replicate n $ getInt xorshift
  let ret = testChisq n k randoms
  return $ ret < limit

spec :: Spec
spec = do
  describe "chisq" $ do
    it "chisq" $ do
      ret <- execTestChisq
      ret `shouldBe` True

