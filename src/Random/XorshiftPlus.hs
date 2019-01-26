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
  ( XorshiftPlus
  , genXorshiftPlusWord
  , genXorshiftPlusInt
  , getWord
  , getInt
  , getDouble
  ) where

import Data.IORef
-- import Data.Coerce
import GHC.Types
import GHC.Prim
import Prelude hiding (not)

data XorshiftPlus1 = XorshiftPlus1 Word# Word#

-- | Random state
newtype XorshiftPlus = XorshiftPlus (IORef XorshiftPlus1)

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

-- | Generate a new random state by a Word seed.
genXorshiftPlusWord
  :: Word            -- ^ random seed
  -> IO XorshiftPlus
genXorshiftPlusWord (W# w) = do
  ref <- newIORef (XorshiftPlus1 w (not w))
  return $ XorshiftPlus ref

-- | Generate a new random state by an Int seed.
genXorshiftPlusInt
  :: Int             -- ^ random seed
  -> IO XorshiftPlus
genXorshiftPlusInt i = genXorshiftPlusWord (fromIntegral i)

-- | Get a new random value as Word.
getWord :: XorshiftPlus -> IO Word
getWord (XorshiftPlus ref) = atomicModifyIORef' ref update
  where
    update :: XorshiftPlus1 -> (XorshiftPlus1, Word)
    update (XorshiftPlus1 w0 w1) =
      let x = w0 `xor` (w0 `shiftL` 17#) in
      let y = x `xor` w1 `xor` (x `shiftR` 17#) `xor` (w1 `shiftR` 26#) in
      (XorshiftPlus1 w1 y, W# (w1 `plus` y))

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
