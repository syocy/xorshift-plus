{-# LANGUAGE TypeApplications #-}

module Main where

import Gauge.Main
import Data.Int
import Data.Word
import System.Random
import Random.XorshiftPlus
import Random.Xorshift.Int32
import Random.Xorshift.Int64
import qualified System.Random.Xorshift128Plus as X128
import System.CPUTime

size :: Int
size = 1000

getSeed :: Num a => IO a
getSeed = fromInteger `fmap` getCPUTime

getRandomIntList :: Int -> IO [Int]
getRandomIntList n = do
  seed <- getSeed @ Int
  xorshift <- genXorshiftPlusInt seed
  sequence $ replicate n $ getInt xorshift

getRandomWordList :: Int -> IO [Word]
getRandomWordList n = do
  seed <- getSeed @ Word
  xorshift <- genXorshiftPlusWord seed
  sequence $ replicate n $ getWord xorshift

getRandomIntList_xorshift_Int32 :: Int -> IO [Int32]
getRandomIntList_xorshift_Int32 n = do
  seed <- getSeed @ Int32
  let xorshift = makeXorshift32 seed
  return $ take n $ randoms xorshift

getRandomIntList_xorshift_Int64 :: Int -> IO [Int64]
getRandomIntList_xorshift_Int64 n = do
  seed <- getSeed @ Int64
  let xorshift = makeXorshift64 seed
  return $ take n $ randoms xorshift

getRandomWord64List_Xorshift128Plus :: Int -> IO [Word64]
getRandomWord64List_Xorshift128Plus n = do
  seed <- getSeed @ Word64
  let xorshift = X128.initialize seed
  return $ take n $ makeList xorshift
  where
    makeList gen = let (x, gen') = X128.next gen in x : (makeList gen')

main :: IO ()
main = defaultMain
  [ bgroup "compareWithOtherPRNGs"
    [ bench "xorshift-plus_Int (THIS PACKAGE)" $ nfIO (getRandomIntList size)
    , bench "xorshift-plus_Word (THIS PACKAGE)" $ nfIO (getRandomWordList size)
    , bench "xorshift_Int32" $ nfIO (getRandomIntList_xorshift_Int32 size)
    , bench "xorshift_Ina64" $ nfIO (getRandomIntList_xorshift_Int64 size)
    , bench "Xorshift128Plus_Word64" $ nfIO (getRandomWord64List_Xorshift128Plus size)
    ]
  ]
