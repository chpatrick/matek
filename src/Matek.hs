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

type IsMatrix row col a = (Space row, Space col, Scalar a)

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

mMap :: (Space row, Space col, Scalar a, Scalar b) => (a -> b) -> Matrix row col a -> Matrix row col b
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

createCM :: forall row col a. IsMatrix row col a => (CMatrix 'RW a -> IO ()) -> Matrix row col a
createCM populate =
  createM $ \mba ->
    case mutableByteArrayContents mba of
      Addr addr -> unsafePrimToPrim $ populate CMatrix
        { cmData = Ptr addr
        , cmRows = fromIntegral (dims @row)
        , cmCols = fromIntegral (dims @col)
        }
{-# INLINE createCM #-}

unsafeWithCM :: IsMatrix row col a => Matrix row col a -> (CMatrix 'R a -> IO b) -> IO b
unsafeWithCM m f = do
  unsafeWithM m $ \mPtr ->
    f CMatrix
      { cmData = castPtr mPtr
      , cmRows = fromIntegral (rows m)
      , cmCols = fromIntegral (cols m)
      }
{-# INLINE unsafeWithCM #-}

binopCM :: ( Space s1, Space s2, Space s3, Space s4, Space s5, Space s6
           , Scalar a, Scalar b, Scalar r
           ) => (CMatrix 'RW r -> CMatrix 'R a -> CMatrix 'R b -> IO ()) -> Matrix s1 s2 a -> Matrix s3 s4 b -> Matrix s5 s6 r
binopCM f x y =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      unsafeWithCM y $ \cy ->
        f cr cx cy
{-# INLINE binopCM #-}

unopCM :: ( Space s1, Space s2, Space s3, Space s4
          , Scalar a, Scalar r
          ) => (CMatrix 'RW r -> CMatrix 'R a -> IO ()) -> Matrix s1 s2 a -> Matrix s3 s4 r
unopCM f x =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      f cr cx
{-# INLINE unopCM #-}

(!*!) :: (Space s1, Space s2, Space s3, Scalar a) => Matrix s3 s2 a -> Matrix s2 s1 a -> Matrix s3 s1 a
(!*!) = binopCM cmMul

instance (KnownNat rows, KnownNat cols, Scalar a, Num a) => Num (Matrix (S rows) (S cols) a) where
  (+) = binopCM cmPlus
  (-) = binopCM cmMinus
  (*) = binopCM cmMul
  abs = unopCM cmAbs
  signum = unopCM cmSignum
  fromInteger x = fromList $ replicate (nRows * nCols) (fromInteger x)
    where
      nRows = fromIntegral $ natVal (Proxy @rows)
      nCols = fromIntegral $ natVal (Proxy @cols)

tr :: IsMatrix row col a => Matrix row col a -> Matrix col row a
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
  doFromBlocks :: HasCallStack => (CMatrix 'RW (FromBlocksScalar a) -> IO ( Maybe ( CSize, CSize ), CSize )) -> a

instance IsMatrix row col a => FromBlocks (Matrix row col a) where
  type FromBlocksScalar (Matrix row col a) = a
  doFromBlocks copyBlocks = createCM $ \r -> do
    copyResult <- copyBlocks r
    case copyResult of
      ( Nothing, finalRow ) ->
        when (finalRow < fromIntegral (untag (dims @row))) $ error "Didn't provide enough rows."
      _ -> error "Didn't provide enough blocks."

instance (IsMatrix row col a, FromBlocks fb, a ~ FromBlocksScalar fb) => FromBlocks (Matrix row col a -> fb) where
  type FromBlocksScalar (Matrix row col a -> next) = FromBlocksScalar next
  doFromBlocks copyBlocks m = doFromBlocks $ \res -> do
    ( m'colAndBlockSize, currentRow ) <- copyBlocks res

    let CMatrix { cmRows = resRows, cmCols = resCols } = res
    unsafeWithCM m $ \cm@CMatrix{..} -> do
      let currentCol = case m'colAndBlockSize of
            Nothing -- we're starting a new row of blocks
              | currentRow + cmRows > resRows -> error "Too many rows provided."
              | otherwise -> 00
            Just ( col, blockSize ) -- we're in a row of blocks
              | blockSize /= cmRows -> error "Inconsistent block row numbers."
              | otherwise -> col
      when (currentCol + cmCols > resCols) $
        error "Too many columns specified."
      cmCopyBlock res currentRow currentCol cm
      return $ if currentCol + cmCols == resCols
        then ( Nothing, (currentRow + cmRows) )
        else ( Just ( currentCol + cmCols, cmRows ), currentRow )

fromBlocks :: (HasCallStack, FromBlocks fb) => fb
fromBlocks = doFromBlocks (\_ -> pure ( Nothing, 0 ))
