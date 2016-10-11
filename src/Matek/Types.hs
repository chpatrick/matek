{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Matek.Types
  ( CMatrix(..)
  , cmRows
  , cmCols
  , CEigenException(..)
  , matekCatch
  , Scalar(..)
  , MatrixSpec(..)
  , ShowScalar(..)
  , showRealFloat
  , Access(..)
  , Min'
  , Min
  , Decomposable(..)
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Primitive
import           Data.Proxy
import           Data.Tagged
import           Foreign.C
import           GHC.Stack
import           GHC.Ptr
import           GHC.Types
import           GHC.TypeLits
import           Numeric

data Access = R | RW
  deriving (Eq, Ord, Show)

-- | A C-friendly reference to @M a@.
newtype CMatrix (acc :: Access) (rows :: Nat) (cols :: Nat) a = CMatrix
  { cmData :: Ptr (CScalar a)
  }

cmRows :: forall acc rows cols a. KnownNat rows => CMatrix acc rows cols a -> CSize
cmRows _ = fromIntegral (natVal (Proxy @rows))

cmCols :: forall acc rows cols a. KnownNat cols => CMatrix acc rows cols a -> CSize
cmCols _ = fromIntegral (natVal (Proxy @cols))

type BinOpCM row col a = CMatrix 'RW row col a -> CMatrix 'R row col a -> CMatrix 'R row col a -> IO ()
type UnOpCM row col a = CMatrix 'RW row col a -> CMatrix 'R row col a -> IO ()

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

-- Specialized operations that produce a matrix of a certain size.
class (Scalar a, KnownNat row, KnownNat col) => MatrixSpec row col a where
  -- Specialized operations on CMatrix a
  cmPlus :: BinOpCM row col a
  cmMinus :: BinOpCM row col a
  cmCoeffMul :: BinOpCM row col a
  cmGenMul :: KnownNat n => CMatrix 'RW row col a -> CMatrix 'R row n a -> CMatrix 'R n col a -> IO ()
  cmTranspose :: CMatrix 'RW row col a -> CMatrix 'R col row a -> IO ()
  cmAbs :: UnOpCM row col a
  cmSignum :: UnOpCM row col a
  cmMap :: FunPtr (CScalar a -> IO (CScalar a)) -> UnOpCM row col a
  cmScale :: CScalar a -> UnOpCM row col a
  -- cmCopyBlock dst dstRow dstCol src
  cmCopyBlock :: (KnownNat subRow, KnownNat subCol) => CMatrix 'RW row col a -> CSize -> CSize -> CMatrix 'R subRow subCol a -> IO ()

class ShowScalar a where
  showScalar :: a -> String

showRealFloat :: RealFloat a => a -> String
showRealFloat x = showGFloat (Just 4) x ""

type family Min' ord (a :: Nat) (b :: Nat) where
  Min' 'LT a b = a
  Min' 'EQ a b = a
  Min' 'GT a b = b

type Min a b = Min' (CmpNat a b) a b

class Scalar a => Decomposable a where
  -- | Compute U, s, V from M
  cmFullSVD :: (KnownNat m, KnownNat n, KnownNat (Min m n))
            => CMatrix 'RW m n a
            -> CMatrix 'RW m m a
            -> CMatrix 'RW (Min m n) 1 a
            -> CMatrix 'R n n a
            -> IO ()
