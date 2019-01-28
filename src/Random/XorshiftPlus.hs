{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : Random.XorshiftPlus
Description : xorshift+ implementation
Copyright   : (c) OSANAI Kazuyoshi, 2019
License     : BSD 3-Clause
Maintainer  : osmium.k@gmail.com
Stability   : experimental
Portability : GHC, word size 64bit

Simple implementation of xorshift+.

>>> gen <- genXorshiftPlusInt 1
>>> getInt gen
2455688531189531812
-}
module Random.XorshiftPlus
  (
  -- * IO version
    XorshiftPlus
  -- ** Generator
  , genXorshiftPlusWord
  , genXorshiftPlusInt
  -- ** Values
  , getWord
  , getInt
  , getDouble
  -- * Low level, ST version
  , XorshiftPlusST
  , genXorshiftPlusWordST
  , getWordST
  -- * Internal
  , splitMix64Next
  ) where

-- import Data.IORef
import Data.STRef
-- import Data.Coerce
import Control.Monad.ST
import GHC.Types
import GHC.Prim
import Prelude hiding (not)

-- $setup
-- >>> :set -XMagicHash

data XorshiftPlus1 = XorshiftPlus1 Word# Word#

-- | Random state
newtype XorshiftPlusST s = XorshiftPlusST (STRef s XorshiftPlus1)

-- | Random state
type XorshiftPlus = XorshiftPlusST RealWorld

plus :: Word# -> Word# -> Word#
plus = plusWord#

times :: Word# -> Word# -> Word#
times = timesWord#

xor :: Word# -> Word# -> Word#
xor = xor#

shiftL :: Word# -> Int# -> Word#
shiftL = uncheckedShiftL#

shiftR :: Word# -> Int# -> Word#
shiftR = uncheckedShiftRL#

-- | For first return value.
--
-- >>> W# (splitMix64Next 1##)
-- 10451216379200822465
splitMix64Next :: Word# -> Word#
splitMix64Next x =
  let z1 = x `plus` 0x9e3779b97f4a7c15## in
  let z2 = (z1 `xor` (z1 `shiftR` 30#)) `times` 0xbf58476d1ce4e5b9## in
  let z3 = (z2 `xor` (z2 `shiftR` 27#)) `times` 0x94d049bb133111eb## in
  z3 `xor` (z3 `shiftR` 31#)

-- | Generate a new random state by a Word seed.
genXorshiftPlusWordST :: Word -> ST s (XorshiftPlusST s)
genXorshiftPlusWordST (W# w) = do
  ref <- newSTRef (XorshiftPlus1 w (splitMix64Next w))
  return $ XorshiftPlusST ref

-- | Generate a new random state by a Word seed.
genXorshiftPlusWord
  :: Word            -- ^ random seed
  -> IO XorshiftPlus
genXorshiftPlusWord w = stToIO (genXorshiftPlusWordST w)

-- | Generate a new random state by an Int seed.
genXorshiftPlusInt
  :: Int             -- ^ random seed
  -> IO XorshiftPlus
genXorshiftPlusInt i = genXorshiftPlusWord (fromIntegral i)

-- | Get a new random value as Word.
getWordST :: XorshiftPlusST s -> ST s Word
getWordST (XorshiftPlusST ref) = do
  XorshiftPlus1 w0 w1 <- readSTRef ref
  let x = w0 `xor` (w0 `shiftL` 17#)
  let y = x `xor` w1 `xor` (x `shiftR` 17#) `xor` (w1 `shiftR` 26#)
  writeSTRef ref $ XorshiftPlus1 w1 y
  return $ W# (w1 `plus` y)

-- | Get a new random value as Word.
getWord :: XorshiftPlus -> IO Word
getWord = stToIO . getWordST

-- | Get a new random value as Int.
getInt :: XorshiftPlus -> IO Int
getInt x = do
  w <- getWord x
  return $ fromIntegral w

-- | Get a new random value as Double [0, 1.0].
getDouble :: XorshiftPlus -> IO Double
getDouble x = do
  let maxWord = fromIntegral (maxBound :: Word)
  w <- getWord x
  return $ (fromIntegral w) / maxWord
