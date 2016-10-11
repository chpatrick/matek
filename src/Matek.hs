{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Matek
  ( Space(..)
  , dims
  , S
  , Matrix(..)
  , Vector
  , createM
  , unsafeWithM
  , fromList
  , fromListColumnMajor
  , toListColumnMajor
  , fromVectorColumnMajor
  , toVectorColumnMajor
  , rows
  , cols
  , toRows
  , mMap
  , createCM
  , unsafeWithCM
  , tr
  , (!*!)
  , (^*)
  , (*^)
  , FromBlocks
  , fromBlocks
  ) where

import           Control.Monad.Cont
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.List
import           Data.Primitive
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Primitive as VP
import           Foreign.C
import           GHC.Ptr
import           GHC.Stack
import           GHC.TypeLits

import           Matek.Types
import           Matek.Scalars ()

-- | A type representing a vector space, with a statically known number of dimensions.
class KnownNat (Dims s) => Space s where
  type Dims s :: Nat

-- | An "anonymous space" with the given number of dimensions.
data S (dims :: Nat)

instance KnownNat dims => Space (S dims) where
  type Dims (S dims) = dims

dims :: forall s. Space s => Tagged s Int
dims = fromIntegral (natVal (Proxy @(Dims s)))

-- Always an aligned pinned ByteArray
-- in column-major order
newtype Matrix row col a = Matrix ByteArray

type IsMatrix row col a = (Space row, Space col, MatrixSpec (Dims row) (Dims col) a)

type Vector space = Matrix space (S 1)

-- low level operations
createM :: forall row col a. IsMatrix row col a => (forall s. MutableByteArray s -> ST s ()) -> Matrix row col a
createM populate = Matrix $ runST $ do
  let size = untag (sizeofScalar @a) * untag (dims @row) * untag (dims @col)
  ba <- newAlignedPinnedByteArray size 128 -- alignment for SSE
  populate ba
  unsafeFreezeByteArray ba
{-# INLINE createM #-}

unsafeWithM :: PrimMonad m => Matrix row col a -> (Ptr a -> m b) -> m b
unsafeWithM (Matrix ba) f = do
  result <- case byteArrayContents ba of
    Addr addr -> f (Ptr addr)
  touch ba
  return result
{-# INLINE unsafeWithM #-}

-- high level operations
fromList :: forall a row col. IsMatrix row col a => [ a ] -> Matrix row col a
fromList xs = createM $ \mba -> go mba xs indices
  where
    indices =
      [ col * rowDims + row
      | row <- [0..rowDims - 1]
      , col <- [0..colDims - 1]
      ]
    rowDims = untag (dims @row)
    colDims = untag (dims @col)
    elemNumber = rowDims * colDims
    go _ [] [] = return ()
    go mba (y : ys) (ix : is) = do
          writeScalar mba ix y
          go mba ys is
    go _ _ _ = error ("Expected " ++ show elemNumber ++ " elements.")

fromListColumnMajor :: forall row col a. IsMatrix row col a => [ a ] -> Matrix row col a
fromListColumnMajor xs = createM $ \mba -> go mba xs 0
  where
    rowDims = untag (dims @row)
    colDims = untag (dims @col)
    elemNumber = rowDims * colDims
    go _ [] n | n == elemNumber = return ()
    go _ [] _ = error ("Expected " ++ show elemNumber ++ " elements.")
    go mba (y : ys) n | n < elemNumber = do
      writeScalar mba n y
      go mba ys (n + 1)
    go _ _ _ = error ("Expected " ++ show elemNumber ++ " elements.")

mMap :: (IsMatrix row col a, IsMatrix row col b) => (a -> b) -> Matrix row col a -> Matrix row col b
mMap f m@(Matrix ba) =
  createM $ \mba -> forM_ [0..elems - 1] $ \i ->
    writeScalar mba i $ f $ indexScalar ba i
  where
    elems = rows m * cols m

rows :: forall row col a. Space row => Matrix row col a -> Int
rows _ = untag (dims @row)

cols :: forall row col a. Space col => Matrix row col a -> Int
cols _ = untag (dims @col)

toListColumnMajor :: IsMatrix row col a => Matrix row col a -> [ a ]
toListColumnMajor m@(Matrix ba)
  = map (indexScalar ba) [0..rows m * cols m - 1]
{-# INLINE toListColumnMajor #-}

-- Convert a matrix to a column-major Primitive.Vector without copying.
toVectorColumnMajor :: IsMatrix row col a => Matrix row col a -> VP.Vector a
toVectorColumnMajor m@(Matrix ba) = VP.Vector 0 (rows m * cols m) ba

-- Get a matrix from a column-major Primitive.Vector (copying).
fromVectorColumnMajor :: forall row col a. (IsMatrix row col a, Prim a) => VP.Vector a -> Matrix row col a
fromVectorColumnMajor (VP.Vector off n vba)
 | n == elems = createM $ \mba ->
    -- Vector offset is in elements not bytes
    copyByteArray mba 0 vba (off * scalarSize) (elems * scalarSize)
 | otherwise = error $ "Expected " ++ show elems ++ " elements."
  where
    scalarSize = untag (sizeofScalar @a)
    elems = untag (dims @row) * untag (dims @col)

toRows :: IsMatrix row col a => Matrix row col a -> [ [ a ] ]
toRows m@(Matrix ba) =
  [ [ indexScalar ba (col * rows m + row)
    | col <- [0..cols m - 1]
    ]
  | row <- [0..rows m - 1]
  ]

showWith :: IsMatrix row col a => (a -> String) -> Matrix row col a -> String
showWith showA m = intercalate "\n" formattedRows
  where
    elemStringRows :: [[ String ]]
    elemStringRows = map (map showA) (toRows m)
    columnWidths :: [ Int ]
    columnWidths = map (maximum . map length) $ transpose elemStringRows
    formattedRows :: [ String ]
    formattedRows = addBorder $ map formatRow elemStringRows
    formatRow :: [ String ] -> String
    formatRow row = intercalate "  " (zipWith justify columnWidths row)
    justify :: Int -> String -> String
    justify colWidth ns = replicate (colWidth - length ns) ' ' ++ ns
    addBorder :: [ String ] -> [ String ]
    addBorder [] = [ "[]" ]
    addBorder [x] = [ "[" ++ x ++ "]" ]
    addBorder (x : xs) = ("┌" ++ x ++ "┐") : addBorder' xs
    addBorder' [] = []
    addBorder' [x] =  [ "└" ++ x ++ "┘" ]
    addBorder' (x : xs) = ("│" ++ x ++ "│") : addBorder' xs

instance (IsMatrix row col a, ShowScalar a) => Show (Matrix row col a) where
  show = showWith showScalar
  showList xs = showString $ "[\n" ++ intercalate ",\n\n" (map show xs) ++ "\n]"

createCM :: forall row col a. IsMatrix row col a => (CMatrix 'RW (Dims row) (Dims col) a -> IO ()) -> Matrix row col a
createCM populate =
  createM $ \mba ->
    case mutableByteArrayContents mba of
      Addr addr -> unsafePrimToPrim $ populate CMatrix { cmData = Ptr addr}
{-# INLINE createCM #-}

unsafeWithCM :: IsMatrix row col a => Matrix row col a -> (CMatrix 'R (Dims row) (Dims col) a -> IO b) -> IO b
unsafeWithCM m f = do
  unsafeWithM m $ \mPtr -> f CMatrix { cmData = castPtr mPtr }
{-# INLINE unsafeWithCM #-}

binopCM :: (IsMatrix s1 s2 a, IsMatrix s3 s4 b, IsMatrix s5 s6 r)
        => (CMatrix 'RW (Dims s5) (Dims s6) r -> CMatrix 'R (Dims s1) (Dims s2) a -> CMatrix 'R (Dims s3) (Dims s4) b -> IO ())
        -> Matrix s1 s2 a -> Matrix s3 s4 b -> Matrix s5 s6 r
binopCM f x y =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      unsafeWithCM y $ \cy ->
        f cr cx cy
{-# INLINE binopCM #-}

unopCM :: (IsMatrix s1 s2 a, IsMatrix s3 s4 r)
       => (CMatrix 'RW (Dims s3) (Dims s4) r -> CMatrix 'R (Dims s1) (Dims s2) a -> IO ())
       -> Matrix s1 s2 a -> Matrix s3 s4 r
unopCM f x =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      f cr cx
{-# INLINE unopCM #-}

(!*!) :: (IsMatrix s3 s1 a, IsMatrix s2 s1 a, IsMatrix s3 s2 a) => Matrix s3 s2 a -> Matrix s2 s1 a -> Matrix s3 s1 a
(!*!) = binopCM cmGenMul

instance (IsMatrix (S rows) (S cols) a, Num a) => Num (Matrix (S rows) (S cols) a) where
  (+) = binopCM cmPlus
  (-) = binopCM cmMinus
  (*) = binopCM cmCoeffMul
  abs = unopCM cmAbs
  signum = unopCM cmSignum
  fromInteger x = fromList $ replicate (nRows * nCols) (fromInteger x)
    where
      nRows = fromIntegral $ natVal (Proxy @rows)
      nCols = fromIntegral $ natVal (Proxy @cols)

tr :: (IsMatrix row col a, IsMatrix col row a) => Matrix row col a -> Matrix col row a
tr = unopCM cmTranspose
{-# INLINE tr #-}

(*^) :: IsMatrix row col a => a -> Matrix row col a -> Matrix row col a
k *^ m = unopCM (cmScale (toCScalar k)) m
{-# INLINE (*^) #-}

(^*) :: IsMatrix row col a => Matrix row col a -> a -> Matrix row col a
(^*) = flip (*^)
{-# INLINE (^*) #-}

class FromBlocks a where
  type FromBlocksScalar a
  type FromBlocksRows a :: Nat
  type FromBlocksCols a :: Nat
  doFromBlocks :: HasCallStack => (CMatrix 'RW (FromBlocksRows a) (FromBlocksCols a) (FromBlocksScalar a) -> IO ( Maybe ( CSize, CSize ), CSize )) -> a

instance IsMatrix row col a => FromBlocks (Matrix row col a) where
  type FromBlocksScalar (Matrix row col a) = a
  type FromBlocksRows (Matrix row col a) = Dims row
  type FromBlocksCols (Matrix row col a) = Dims col
  doFromBlocks copyBlocks = createCM $ \r -> do
    copyResult <- copyBlocks r
    case copyResult of
      ( Nothing, finalRow ) ->
        when (finalRow < fromIntegral (untag (dims @row))) $ error "Didn't provide enough rows."
      _ -> error "Didn't provide enough blocks."

instance ( IsMatrix row col a
         , MatrixSpec (FromBlocksRows fb) (FromBlocksCols fb) (FromBlocksScalar fb)
         , FromBlocks fb
         , a ~ FromBlocksScalar fb
         ) => FromBlocks (Matrix row col a -> fb) where
  type FromBlocksScalar (Matrix row col a -> next) = FromBlocksScalar next
  type FromBlocksRows (Matrix row col a -> next) = FromBlocksRows next
  type FromBlocksCols (Matrix row col a -> next) = FromBlocksCols next
  doFromBlocks copyBlocks m = doFromBlocks $ \res -> do
    ( m'colAndBlockSize, currentRow ) <- copyBlocks res

    let resRows = cmRows res
    let resCols = cmCols res
    unsafeWithCM m $ \block -> do
      let blockRows = cmRows block
      let currentCol = case m'colAndBlockSize of
            Nothing -- we're starting a new row of blocks
              | currentRow + blockRows > resRows -> error "Too many rows provided."
              | otherwise -> 00
            Just ( col, blockSize ) -- we're in a row of blocks
              | blockSize /= blockRows -> error "Inconsistent block row numbers."
              | otherwise -> col
      when (currentCol + blockRows > resCols) $
        error "Too many columns specified."
      cmCopyBlock res currentRow currentCol block
      return $ if currentCol + blockRows == resCols
        then ( Nothing, (currentRow + blockRows) )
        else ( Just ( currentCol + blockRows, blockRows ), currentRow )

-- | Assemble a matrix from smaller ones by providing them in top-to-bottom, left-to-right order.
--
-- > let v = fromList [1..4] :: Matrix (S 2) (S 2) Double in fromBlocks v v v v :: Matrix (S 4) (S 4) Double
-- > ┌1.0000  2.0000  1.0000  2.0000┐
-- > │3.0000  4.0000  3.0000  4.0000│
-- > │1.0000  2.0000  1.0000  2.0000│
-- > └3.0000  4.0000  3.0000  4.0000┘
--
fromBlocks :: (HasCallStack, FromBlocks fb) => fb
fromBlocks = doFromBlocks (\_ -> pure ( Nothing, 0 ))
