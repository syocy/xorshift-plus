module Main where

import Gauge.Main
import Data.Int
import Data.Word
import System.Random
import Random.XorshiftPlus
import Random.Xorshift.Int32
import Random.Xorshift.Int64
import qualified System.Random.Xorshift128Plus as X128

size :: Int
size = 1000

getRandomIntList :: Int -> Int -> IO [Int]
getRandomIntList n seed = do
  xorshift <- genXorshiftPlusInt seed
  sequence $ replicate n $ getInt xorshift

getRandomWordList :: Int -> Word -> IO [Word]
getRandomWordList n seed = do
  xorshift <- genXorshiftPlusWord seed
  sequence $ replicate n $ getWord xorshift

getRandomIntList_xorshift_Int32 :: Int -> Int -> [Int32]
getRandomIntList_xorshift_Int32 n seed =
  let xorshift = makeXorshift32 seed in
  take n $ randoms xorshift

getRandomIntList_xorshift_Int64 :: Int -> Int -> [Int64]
getRandomIntList_xorshift_Int64 n seed =
  let xorshift = makeXorshift64 seed in
  take n $ randoms xorshift

getRandomWord64List_Xorshift128Plus :: Int -> Word64 -> [Word64]
getRandomWord64List_Xorshift128Plus n seed =
  let xorshift = X128.initialize seed in
  take n $ makeList xorshift
  where
    makeList gen = let (x, gen') = X128.next gen in x : (makeList gen')

main :: IO ()
main = defaultMain [
    bgroup "variableSeeds"
      [ bench "seed_1" $ nfAppIO (getRandomIntList size) 1
      , bench "seed_-1" $ nfAppIO (getRandomIntList size) (-1)
      , bench "seed_INT_MIN" $ nfAppIO (getRandomIntList size) (minBound :: Int)
      , bench "seed_INT_MAX" $ nfAppIO (getRandomIntList size) (maxBound :: Int)
      ],
    bgroup "compareWithOtherPRNGs"
      [ bench "xorshift-plus_Int (THIS PACKAGE)" $ nfAppIO (getRandomIntList size) 1
      , bench "xorshift-plus_Word (THIS PACKAGE)" $ nfAppIO (getRandomWordList size) 1
      , bench "xorshift_Int32" $ nf (getRandomIntList_xorshift_Int32 size) 1
      , bench "xorshift_Int64" $ nf (getRandomIntList_xorshift_Int64 size) 1
      , bench "Xorshift128Plus_Word64" $ nf (getRandomWord64List_Xorshift128Plus size) 1
      ]
  ]
