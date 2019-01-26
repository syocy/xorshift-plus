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

>>> s <- genXorshiftPlusInt 1
>>> getInt s
-274877775873
-}
module Random.XorshiftPlus
  ( XorshiftPlusST
  , XorshiftPlus
  , genXorshiftPlusWordST
  , genXorshiftPlusWord
  , genXorshiftPlusInt
  , getWordST
  , getWord
  , getInt
  , getDouble
  ) where

-- import Data.IORef
import Data.STRef
-- import Data.Coerce
import Control.Monad.ST
import GHC.Types
import GHC.Prim
import Prelude hiding (not)

data XorshiftPlus1 = XorshiftPlus1 Word# Word#
newtype XorshiftPlusST s = XorshiftPlusST (STRef s XorshiftPlus1)

-- | Random state
type XorshiftPlus = XorshiftPlusST RealWorld

not :: Word# -> Word#
not = not#

plus :: Word# -> Word# -> Word#
plus = plusWord#

xor :: Word# -> Word# -> Word#
xor = xor#

shiftL :: Word# -> Int# -> Word#
shiftL = uncheckedShiftL#

shiftR :: Word# -> Int# -> Word#
shiftR = uncheckedShiftRL#

genXorshiftPlusWordST :: Word -> ST s (XorshiftPlusST s)
genXorshiftPlusWordST (W# w) = do
  ref <- newSTRef (XorshiftPlus1 w (not w))
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
