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

module Matek
  ( Space(..)
  , dims
  , S
  , M(..)
  , V
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
  ) where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.List
import           Data.Primitive
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Primitive as VP
import           GHC.Ptr
import           GHC.TypeLits

import           Matek.Types
import           Matek.Scalars ()

class KnownNat (Dims s) => Space s where
  type Dims s :: Nat

data S (dims :: Nat)

instance KnownNat dims => Space (S dims) where
  type Dims (S dims) = dims

dims :: forall s. Space s => Tagged s Int
dims = fromIntegral (natVal (Proxy @(Dims s))) 

-- Always an aligned pinned ByteArray
-- in column-major order
newtype M row col a = M ByteArray

type IsM row col a = (Space row, Space col, Scalar a)

type V space = M space (S 1)

-- low level operations
createM :: forall row col a. IsM row col a => (forall s. MutableByteArray s -> ST s ()) -> M row col a
createM populate = M $ runST $ do
  let size = untag (sizeofScalar @a) * untag (dims @row) * untag (dims @col)
  ba <- newAlignedPinnedByteArray size 128 -- alignment for SSE
  populate ba
  unsafeFreezeByteArray ba
{-# INLINE createM #-}

unsafeWithM :: PrimMonad m => M row col a -> (Ptr a -> m b) -> m b
unsafeWithM (M ba) f = do
  result <- case byteArrayContents ba of
    Addr addr -> f (Ptr addr)
  touch ba
  return result
{-# INLINE unsafeWithM #-}

-- high level operations
fromList :: forall a row col. IsM row col a => [ a ] -> M row col a
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

fromListColumnMajor :: forall row col a. IsM row col a => [ a ] -> M row col a
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

mMap :: IsM row col a => (a -> a) -> M row col a -> M row col a
mMap f = fromListColumnMajor . map f . toListColumnMajor

{-
columnMajor :: forall space a. (Space row, Space col Scalar a) => M space a -> [ a ]
columnMajor (M ba) = map (indexScalar ba) [0..spaceDims - 1]
  where
    spaceDims = untag (dims @space)
-}

rows :: forall row col a. Space row => M row col a -> Int
rows _ = untag (dims @row)

cols :: forall row col a. Space col => M row col a -> Int
cols _ = untag (dims @col)

toListColumnMajor :: IsM row col a => M row col a -> [ a ]
toListColumnMajor m@(M ba)
  = map (indexScalar ba) [0..rows m * cols m - 1]
{-# INLINE toListColumnMajor #-}

-- Convert a matrix to a column-major Primitive.Vector without copying.
toVectorColumnMajor :: IsM row col a => M row col a -> VP.Vector a
toVectorColumnMajor m@(M ba) = VP.Vector 0 (rows m * cols m) ba

-- Get a matrix from a column-major Primitive.Vector (copying).
fromVectorColumnMajor :: forall row col a. (IsM row col a, Prim a) => VP.Vector a -> M row col a
fromVectorColumnMajor (VP.Vector off n vba)
 | n == elems = createM $ \mba ->
    -- Vector offset is in elements not bytes
    copyByteArray mba 0 vba (off * scalarSize) (elems * scalarSize)
 | otherwise = error $ "Expected " ++ show elems ++ " elements."
  where
    scalarSize = untag (sizeofScalar @a)
    elems = untag (dims @row) * untag (dims @col)

toRows :: IsM row col a => M row col a -> [ [ a ] ]
toRows m@(M ba) =
  [ [ indexScalar ba (col * rows m + row)
    | col <- [0..cols m - 1]
    ]
  | row <- [0..rows m - 1]
  ]

showWith :: IsM row col a => (a -> String) -> M row col a -> String
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

instance (IsM row col a, ShowScalar a) => Show (M row col a) where
  show = showWith showScalar
  showList xs = showString $ "[\n" ++ intercalate ",\n\n" (map show xs) ++ "\n]"

createCM :: forall row col a. IsM row col a => (CM 'RW a -> IO ()) -> M row col a
createCM populate = 
  createM $ \mba ->
    case mutableByteArrayContents mba of
      Addr addr -> unsafePrimToPrim $ populate CM
        { cmData = Ptr addr
        , cmRows = fromIntegral (dims @row)
        , cmCols = fromIntegral (dims @col)
        }
{-# INLINE createCM #-}

unsafeWithCM :: IsM row col a => M row col a -> (CM 'R a -> IO b) -> IO b
unsafeWithCM m f = do
  unsafeWithM m $ \mPtr ->
    f CM 
      { cmData = castPtr mPtr
      , cmRows = fromIntegral (rows m)
      , cmCols = fromIntegral (cols m)
      }
{-# INLINE unsafeWithCM #-}

binopCM :: ( Space s1, Space s2, Space s3, Space s4, Space s5, Space s6
           , Scalar a, Scalar b, Scalar r
           ) => (CM 'RW r -> CM 'R a -> CM 'R b -> IO ()) -> M s1 s2 a -> M s3 s4 b -> M s5 s6 r
binopCM f x y =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      unsafeWithCM y $ \cy ->
        f cr cx cy
{-# INLINE binopCM #-}

unopCM :: ( Space s1, Space s2, Space s3, Space s4
          , Scalar a, Scalar r
          ) => (CM 'RW r -> CM 'R a -> IO ()) -> M s1 s2 a -> M s3 s4 r
unopCM f x =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      f cr cx
{-# INLINE unopCM #-}

(!*!) :: (Space s1, Space s2, Space s3, Scalar a) => M s3 s2 a -> M s2 s1 a -> M s3 s1 a
(!*!) = binopCM cmMul

instance (KnownNat rows, KnownNat cols, Scalar a, Num a) => Num (M (S rows) (S cols) a) where
  (+) = binopCM cmPlus
  (-) = binopCM cmMinus
  (*) = binopCM cmMul
  abs = unopCM cmAbs
  signum = unopCM cmSignum
  fromInteger x = fromList $ replicate (nRows * nCols) (fromInteger x)
    where
      nRows = fromIntegral $ natVal (Proxy @rows)
      nCols = fromIntegral $ natVal (Proxy @cols)

tr :: IsM row col a => M row col a -> M col row a
tr = unopCM cmTranspose
{-# INLINE tr #-}

(*^) :: IsM row col a => a -> M row col a -> M row col a
k *^ m = unopCM (cmScale (toCScalar k)) m
{-# INLINE (*^) #-}

(^*) :: IsM row col a => M row col a -> a -> M row col a
(^*) = flip (*^)
{-# INLINE (^*) #-}