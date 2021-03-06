{-# LANGUAGE TypeApplications #-}

module Random.XorshiftPlusSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import Random.XorshiftPlus
import Data.List
import Control.Monad
import System.CPUTime

run :: IO ()
run = do
  seed <- getCPUTime
  xorshift <- genXorshiftPlusInt (fromInteger seed)
  ws <- sequence $ replicate 10 $ getWord xorshift
  print ws
  is <- sequence $ replicate 10 $ getInt xorshift
  print is
  ds <- sequence $ replicate 10 $ getDouble xorshift
  print ds

rangeDouble :: Property
rangeDouble = property $ \seed -> QC.monadicIO $ do
  xorshift <- QC.run $ genXorshiftPlusInt seed
  ret <- QC.run $ sequence $ replicate 100 $ getDouble xorshift
  QC.assert $ all (\r -> 0.0 <= r && r <= 1.0) ret

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

testCor :: Int -> Int -> [Double] -> Double
testCor n k randoms = let s = testCor' n randoms 0.0 in
                      let n' = fromIntegral n in
                      sqrt n' * (12.0 * s / n' - 3.0) / sqrt 13.0
  where
    testCor' :: Int -> [Double] -> Double -> Double
    testCor' 0 _       res = res
    testCor' n' randoms' res =
      let (buff, restRandoms) = splitAt k randoms' in
      let x =  head buff * head restRandoms in
      testCor' (n'-1) (tail randoms') (res+x)

execTestCor :: IO Bool
execTestCor = do
  xorshift <- genXorshiftPlusInt 1
  let n = 2500
  randoms <- sequence $ replicate ((n+1) * 3) $ getDouble xorshift
  cors <- forM [1, 2, 3] $ \k -> do
    let x = testCor n k randoms
    return x
  return $ all (\x -> (-1.96 < x && x < 1.96)) cors

spec :: Spec
spec = do
  describe "_run" $ do
    it "_run" $ do
      run
  describe "range" $ do
    it "getDouble" $ do
      rangeDouble
  describe "chisq" $ do
    it "chisq" $ do
      ret <- execTestChisq
      ret `shouldBe` True
  describe "cor" $ do
    it "cor" $ do
      ret <- execTestCor
      ret `shouldBe` True
