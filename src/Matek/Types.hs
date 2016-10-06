{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Matek.Types
  ( Scalar(..)
  , Access(..)
  , CM(..)
  ) where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Tagged
import           Foreign.C
import           GHC.Ptr
import           GHC.Types

type BinOpCM a = CM RW a -> CM R a -> CM R a -> IO ()
type UnOpCM a = CM RW a -> CM R a -> IO ()

class Scalar a where
  -- | The C equivalent of a (ie. Double -> CDouble).
  type CScalar a

  -- Methods from Prim
  sizeofScalar :: Tagged a Int
  default sizeofScalar :: Prim a => Tagged a Int
  sizeofScalar = Tagged $ I# (sizeOf# (undefined :: a))

  readScalar :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m a
  default readScalar :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
  readScalar = readByteArray

  writeScalar :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> a -> m ()
  default writeScalar :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
  writeScalar = writeByteArray

  indexScalar :: ByteArray -> Int -> a
  default indexScalar :: Prim a => ByteArray -> Int -> a
  indexScalar = indexByteArray

  -- Specialized operations on CM a
  cmPlus :: BinOpCM a
  cmMinus :: BinOpCM a
  cmMul :: BinOpCM a
  cmTranspose :: UnOpCM a
  cmAbs :: UnOpCM a
  cmSignum :: UnOpCM a
  cmMap :: FunPtr (CScalar a -> IO (CScalar a)) -> UnOpCM a

data Access = R | RW
  deriving (Eq, Ord, Show) 

-- | A C-friendly reference to @M a@.
data CM (acc :: Access) a = CM
  { mData :: Ptr (CScalar a)
  , mRows :: CSize
  , mCols :: CSize
  }