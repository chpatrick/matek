{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Matek.Types
  ( CMatrix(..)
  , CEigenException(..)
  , matekCatch
  , Scalar(..)
  , Decomposable(..)
  , ShowScalar(..)
  , showRealFloat
  , Access(..)
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Primitive
import           Data.Tagged
import           Foreign.C
import           GHC.Stack
import           GHC.Ptr
import           GHC.Types
import           Numeric

data Access = R | RW
  deriving (Eq, Ord, Show)

-- | A C-friendly reference to @M a@.
data CMatrix (acc :: Access) a = CMatrix
  { cmData :: {-# UNPACK #-} (Ptr (CScalar a))
  , cmRows :: {-# UNPACK #-} CSize
  , cmCols :: {-# UNPACK #-} CSize
  }

type BinOpCM a = CMatrix 'RW a -> CMatrix 'R a -> CMatrix 'R a -> IO ()
type UnOpCM a = CMatrix 'RW a -> CMatrix 'R a -> IO ()

newtype CEigenException = CEigenException CString

matekCatch :: HasCallStack => IO CEigenException -> IO ()
matekCatch m = do
  CEigenException res <- m
  when (res /= nullPtr) $ do
    msg <- peekCString res
    error msg

class Scalar a where
  -- | The C equivalent of a (ie. Double -> CDouble).
  -- We assume the Storable representation of a and CScalar a is the same.
  type CScalar a

  toCScalar :: a -> CScalar a
  default toCScalar :: Coercible a (CScalar a) => a -> CScalar a
  toCScalar = coerce

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

  -- Specialized operations on CMatrix a
  cmPlus :: BinOpCM a
  cmMinus :: BinOpCM a
  cmMul :: BinOpCM a
  cmTranspose :: UnOpCM a
  cmAbs :: UnOpCM a
  cmSignum :: UnOpCM a
  cmMap :: FunPtr (CScalar a -> IO (CScalar a)) -> UnOpCM a
  cmScale :: CScalar a -> UnOpCM a
  -- cmCopyBlock dst dstRow dstCol src
  cmCopyBlock :: CMatrix 'RW a -> CSize -> CSize -> CMatrix 'R a -> IO ()

class ShowScalar a where
  showScalar :: a -> String

showRealFloat :: RealFloat a => a -> String
showRealFloat x = showGFloat (Just 4) x ""

class Scalar a => Decomposable a where
  -- | Compute U, s, V from M
  cmFullSVD :: CMatrix 'RW a -> CMatrix 'RW a -> CMatrix 'RW a -> CMatrix 'R a -> IO ()