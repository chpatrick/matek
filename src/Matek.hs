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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Matek
  ( Space(..)
  , dims
  , S
  , Matrix(..)
  , Deferred(..)
  , IsMatrix
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
  , HasNum
  , FromBlocks
  , fromBlocks
  , CoercibleSpace
  , coerceMatrix
  , (!*!)
  , (^*)
  , (*^)
  , identity
  , mapRowMajor
  , tr
  ) where

import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.List
import           Data.Primitive
import           Data.Proxy
import           Data.Tagged
import qualified Data.Vector.Generic as VG
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

instance (Eq a, IsMatrix row col a) => Eq (Matrix row col a) where
  mx@(Matrix x) == Matrix y = all (\i -> indexScalar @a x i == indexScalar y i) [0..rows mx * cols mx - 1]

instance (Ord a, IsMatrix row col a) => Ord (Matrix row col a) where
  mx@(Matrix x) `compare` Matrix y = go 0
    where
      go i
        | i >= rows mx * cols mx = EQ
        | otherwise = case indexScalar @a x i `compare` indexScalar y i of
          LT -> LT
          GT -> GT
          EQ -> go (i + 1)

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

showWith :: (a -> String) -> [ [ a ] ] -> String
showWith showA matRows = intercalate "\n" formattedRows
  where
    elemStringRows :: [[ String ]]
    elemStringRows = map (map showA) matRows
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
  show = showWith showScalar . toRows
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

_unopCM :: ( Space s1, Space s2, Space s3, Space s4
          , Scalar a, Scalar r
          ) => (CMatrix 'RW r -> CMatrix 'R a -> IO ()) -> Matrix s1 s2 a -> Matrix s3 s4 r
_unopCM f x =
  createCM $ \cr ->
    unsafeWithCM x $ \cx ->
      f cr cx
{-# INLINE _unopCM #-}

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

type CoercibleSpace row col otherRow otherCol = ((Dims row * Dims col) ~ (Dims otherRow * Dims otherCol))

coerceMatrix :: CoercibleSpace row col otherRow otherCol => Matrix row col a -> Matrix otherRow otherCol a
coerceMatrix (Matrix ba) = Matrix ba

newtype Deferred row col a = Deferred (Tagged row Int -> Tagged col Int -> a)
  deriving (Functor)

evalRowMajor :: forall row col a. (Space row, Space col) => Deferred row col a -> [ [ a ] ]
evalRowMajor (Deferred f) =
  [ [ f row col | col <- [pure 0 .. (subtract 1) <$> dims @col] ]
  | row <- [pure 0 .. (subtract 1) <$> dims @row]
  ]

instance Applicative (Deferred row col) where
  pure x = Deferred $ \_ _ -> x
  {-# INLINE pure #-}

  Deferred f <*> Deferred x = Deferred $ \row col -> f row col (x row col)
  {-# INLINE (<*>) #-}

instance Num a => Num (Deferred row col a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  negate = fmap negate
  {-# INLINE negate #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

-- | Folds over the `Deferred` in row-major order.
instance (Space row, Space col) => Foldable (Deferred row col) where
  foldMap f (Deferred df) = mconcat
    [ f (df row col)
    | row <- [0..dims @row - 1]
    , col <- [0..dims @col - 1]
    ]

instance (ShowScalar a, Space row, Space col) => Show (Deferred row col a) where
  show = showWith showScalar . evalRowMajor
  showList xs = showString $ "[\n" ++ intercalate ",\n\n" (map show xs) ++ "\n]"

-- | Create a `Matrix` from a row-major representation in any `VG.Vector`.
mapRowMajor :: forall v a row col. (VG.Vector v a, IsMatrix row col a) => v a -> Matrix row col a
mapRowMajor v
  | fromIntegral (VG.length v) == untag (dims @row) * untag (dims @col) =
      build $ Deferred $ \row col -> v `VG.unsafeIndex` fromIntegral (untag row * untag (dims @col) + untag col)
  | otherwise = error "Incorrect number of matrix elements in mapped vector."
{-# INLINE mapRowMajor #-}

-- | Convert a realized matrix to a deferred representation.
defer :: forall row col a. IsMatrix row col a => Matrix row col a -> Deferred row col a
defer (Matrix ba)
  = Deferred $ \row col -> indexScalar ba (untag row * untag (dims @col) + untag col)
{-# INLINE[0] defer #-}

-- | Convert a deferred matrix to a realized one, evaluating all components.
realize :: forall row col a. IsMatrix row col a => Deferred row col a -> Matrix row col a
realize (Deferred f)
  = createM $ \mba -> go mba 0 0 0
    where
      go :: MutableByteArray s -> Tagged row Int -> Tagged col Int -> Int -> ST s ()
      go mba row col i
        | col == dims @col = return ()
        | otherwise = do
            writeScalar mba i (f row col)
            if row == dims @row - 1
              then go mba 0 (col + 1) (i + 1)
              else go mba (row + 1) col (i + 1)

build :: forall row col a. IsMatrix row col a => Deferred row col a -> Matrix row col a
build = realize
{-# INLINE[1] build #-}

{-# RULES "defer/build" forall d. defer (build d) = d #-}

-- | Lift a unary `Deferred` operation to a `Matrix` operation.
unopDef :: (Space s1, Space s2, Space s3, Space s4, Scalar a, Scalar b)
         => (Deferred s1 s2 a -> Deferred s3 s4 b)
         -> Matrix s1 s2 a -> Matrix s3 s4 b
unopDef df x = build (df (defer x))
{-# INLINE unopDef #-}

-- | Lift a binary `Deferred` operation to a `Matrix` operation.
binopDef :: (Space s1, Space s2, Space s3, Space s4, Space s5, Space s6, Scalar a, Scalar b, Scalar c)
         => (Deferred s1 s2 a -> Deferred s3 s4 b -> Deferred s5 s6 c)
         -> Matrix s1 s2 a -> Matrix s3 s4 b -> Matrix s5 s6 c
binopDef df x y = build (defer x `df` defer y)
{-# INLINE binopDef #-}

type family HasNum row col where
  HasNum (S rows) col = 'True
  HasNum row (S cols) = 'True
  HasNum row col = 'False

instance (IsMatrix row col a, HasNum row col ~ 'True, Num a) => Num (Matrix row col a) where
  (+) = binopDef (+)
  {-# INLINE (+) #-}
  (-) = binopDef (-)
  {-# INLINE (-) #-}
  (*) = binopDef (*)
  {-# INLINE (*) #-}
  abs = unopDef abs
  {-# INLINE abs #-}
  signum = unopDef signum
  {-# INLINE signum #-}
  negate = unopDef negate
  {-# INLINE negate #-}
  fromInteger = build . fromInteger
  {-# INLINE fromInteger #-}

(!*!) :: (Space s1, Space s2, Space s3, Scalar a) => Matrix s3 s2 a -> Matrix s2 s1 a -> Matrix s3 s1 a
(!*!) = binopCM cmMul

(^*) :: (IsMatrix row col a, Num a) => Matrix row col a -> a -> Matrix row col a
m ^* k  = unopDef (fmap (*k)) m
{-# INLINE (^*) #-}

(*^) :: (IsMatrix row col a, Num a) => a -> Matrix row col a -> Matrix row col a
(*^) = flip (^*)
{-# INLINE (*^) #-}

tr :: IsMatrix row col a => Matrix row col a -> Matrix col row a
tr = unopDef $ \(Deferred f) -> Deferred (\row col -> f col row)
{-# INLINE tr #-}

identity :: (IsMatrix space space a, Num a) => Matrix space space a
identity = build $ Deferred $ \row col -> if untag row == untag col then 1 else 0
{-# INLINE identity #-}
