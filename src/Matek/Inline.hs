{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Matek.Inline
  ( mkSpec
  , mkSpecs
  , mkDecomposable
  , blockMap
  , matekCtx
  ) where

import qualified Data.Map as M
import           Data.Monoid
import           Data.Proxy
import           Data.Traversable
import           GHC.TypeLits
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token
import qualified Text.RawString.QQ as RS

import           Matek.Types

data Token
  = CharToken Char
  | TypeQuoteToken String
  | MapQuoteToken String Access
    deriving (Eq, Ord, Show)

tokenize :: String -> [ Token ]
tokenize s = case parse (many $ try parseMapQuote <|> try parseTypeQuote <|> (CharToken <$> anyChar)) "" s of
  Left err -> error $ "impossibly failed to tokenize: " ++ show err
  Right ts -> ts
  where
    TokenParser {..} = makeTokenParser haskellDef
    parseMapQuote = do
      _ <- string "$map"
      acc <- (RW <$ try (string "RW")) <|> (R <$ string "R")
      _ <- string "("
      cmName <- identifier
      _ <- string ")"
      return $ MapQuoteToken cmName acc
    parseTypeQuote = do
      _ <- string "$type("
      cmName <- identifier
      _ <- string ")"
      return $ TypeQuoteToken cmName

assertAccessType :: Proxy acc -> Proxy a -> CMatrix acc row col a -> CMatrix acc row col a
assertAccessType _ _ cm = cm
{-# INLINE assertAccessType #-}

assertSize :: Proxy row -> Proxy col -> CMatrix acc row col a -> CMatrix acc row col a
assertSize _ _ cm = cm
{-# INLINE assertSize #-}

-- like C.block but $map(r) (where r is a CMatrix a) gets replaced with an Eigen Map with the given element type
blockMap :: String -> (String -> ( TypeQ, String, Maybe ( Int, Int ) )) -> Q Exp
blockMap cBlock mapTypeOf = do
  let cBlockTokens = tokenize cBlock
  blockResults <- for cBlockTokens $ \case
    CharToken c -> return ( [ c ], [] )
    MapQuoteToken cmName acc -> do
      let ptrName = mkName (cmName ++ "_cmPtr_inline")
      let ( scalarType, cType, m'staticSize ) = mapTypeOf cmName
      let accType = case acc of
            R -> [t|'R|]
            RW -> [t|'RW|]
      let baseCmExp = varE (mkName cmName)
      case m'staticSize of
        Nothing -> do
          let cmExpName = mkName (cmName ++ "_cm_inline")
          let rowsName = mkName (cmName ++ "_cmRows_inline")
          let colsName = mkName (cmName ++ "_cmCols_inline")
          let replacement = concat
                [ "Map<"
                , case acc of { RW -> ""; R -> "const " }
                , "Matrix<"
                , cType
                , ", Dynamic, Dynamic>>($("
                , cType
                , "* "
                , nameBase ptrName
                , "), $(size_t "
                , nameBase rowsName
                , "), $(size_t "
                , nameBase colsName
                , "))"
                ]
          let binds =
                [ ( cmExpName, [e|assertAccessType (Proxy :: Proxy $(accType)) (Proxy :: Proxy $(scalarType)) $(baseCmExp)|] )
                , ( ptrName, [e|cmData $(varE cmExpName)|] )
                , ( rowsName, [e|cmRows $(varE cmExpName)|] )
                , ( colsName, [e|cmCols $(varE cmExpName)|] )
                ]
          return ( replacement, binds )
        Just ( staticRows, staticCols ) -> do
          let replacement = concat
                [ "Map<"
                , case acc of { RW -> ""; R -> "const " }
                , "Matrix<"
                , cType
                , ", " ++ show staticRows ++ ", " ++ show staticCols ++ ">>($("
                , cType
                , "* "
                , nameBase ptrName
                , "))"
                ]
          let rowsType = litT $ numTyLit $ fromIntegral staticRows
          let colsType = litT $ numTyLit $ fromIntegral staticCols
          let cmExp = [e|
                assertSize (Proxy :: Proxy $(rowsType)) (Proxy :: Proxy $(colsType)) $
                  assertAccessType (Proxy :: Proxy $(accType)) (Proxy :: Proxy $(scalarType))
                    $(baseCmExp)
                |]
          let binds =
                [ ( ptrName, [e|cmData $(cmExp)|] )
                ]
          return ( replacement, binds )
    TypeQuoteToken cmName -> do
      let ( _ , cType, _ ) = mapTypeOf cmName
      return ( cType, [] )
  let ( translatedCBlockStrings, cmBlockBinds ) = unzip blockResults
  let translatedCBlock = concat translatedCBlockStrings
  let bindCMs = letS [ valD (varP varName) (normalB expr) [] | cmBinds <- cmBlockBinds, ( varName, expr ) <- cmBinds ]
  doE [ bindCMs, noBindS (quoteExp CU.block translatedCBlock) ]

mkSpec :: TypeQ -> String -> Maybe ( Int, Int ) -> DecsQ
mkSpec scalarType cType m'staticSize =
  case m'staticSize of
    Nothing ->
      [d|
        instance {-# OVERLAPPABLE #-} (KnownNat rows, KnownNat cols) => MatrixSpec rows cols $(scalarType) where
          cmPlus = $qcmPlus
          cmMinus = $qcmMinus
          cmGenMul = $qcmGenMul
          cmCoeffMul = $qcmCoeffMul
          cmTranspose = $qcmTranspose
          cmAbs = $qcmAbs
          cmMap = $qcmMap
          cmSignum = $qcmSignum
          cmScale = $qcmScale
          cmCopyBlock = $qcmCopyBlock
        |]
    Just ( rows, cols ) ->
      [d|
        instance {-# OVERLAPPING #-} MatrixSpec $(rowsType) $(colsType) $(scalarType) where
          cmPlus = $qcmPlus
          cmMinus = $qcmMinus
          cmGenMul = $qcmGenMul
          cmCoeffMul = $qcmCoeffMul
          cmTranspose = $qcmTranspose
          cmAbs = $qcmAbs
          cmMap = $qcmMap
          cmSignum = $qcmSignum
          cmScale = $qcmScale
          cmCopyBlock = $qcmCopyBlock
        |]
        where
          rowsType = litT $ numTyLit $ fromIntegral rows
          colsType = litT $ numTyLit $ fromIntegral cols

  where
    qcmPlus = [e|\r x y -> $(blockMap [RS.r| void {
        auto r= $mapRW(r);
        r = $mapR(x) + $mapR(y);
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmMinus = [e|\r x y -> $(blockMap [RS.r| void {
        auto r = $mapRW(r);
        r = $mapR(x) - $mapR(y);
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmGenMul = [e|\r x y -> $(blockMap [RS.r| void {
        auto r = $mapRW(r);
        r = $mapR(x) * $mapR(y);
      } |] $ \case
        "r" -> ( scalarType, cType, m'staticSize )
        "x" -> ( scalarType, cType, Nothing )
        "y" -> ( scalarType, cType, Nothing )
        _ -> error "Unexpected parameter."
        ) |]

    qcmCoeffMul = [e|\r x y -> $(blockMap [RS.r| void {
        $mapRW(r).array() = $mapR(x).array() * $mapR(y).array();
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmTranspose = [e|\r x -> $(blockMap [RS.r| void {
        auto r = $mapRW(r);
        r = $mapR(x).transpose();
      } |] $ \case
        "r" -> ( scalarType, cType, m'staticSize )
        "x" -> ( scalarType, cType, (\( rows, cols ) -> ( cols, rows )) <$> m'staticSize )
        _ -> error "Unexpected parameter."
        ) |]

    qcmAbs = [e|\r x -> $(blockMap [RS.r| void {
        $mapRW(r).array() = $mapR(x).array().abs();
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmMap = [e|\f r x -> $(blockMap [RS.r| void {
        auto r = $mapRW(r);
        r = $mapR(x).unaryExpr(std::ptr_fun($($type(x) (*f)($type(x)) )));
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmSignum = [e|\r x -> $(blockMap [RS.r| void {
        auto r = $mapRW(r);
        auto signum = []($type(x) n) { return n > 0 ? 1 : (n == 0 ? 0 : -1); };
        r = $mapR(x).unaryExpr(signum);
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmScale = [e|\k r x -> $(blockMap [RS.r| void {
        auto r = $mapRW(r);
        r = $mapR(x) * $($type(x) k);
      } |] (const ( scalarType, cType, m'staticSize ))) |]

    qcmCopyBlock = [e|\r dstRow dstCol x -> $(blockMap [RS.r| void {
        auto src = $mapR(x);
        $mapRW(r).block($(size_t dstRow), $(size_t dstCol), src.rows(), src.cols()) = src;
      } |] $ \case
        "r" -> ( scalarType, cType, m'staticSize )
        "x" -> ( scalarType, cType, Nothing )
        _ -> error "Unexpected parameter."
        ) |]

mkSpecs :: TypeQ -> String -> [ ( Int, Int ) ] -> DecsQ
mkSpecs scalarType cType staticSizes = do
  decs <- traverse (mkSpec scalarType cType) (Nothing : map Just staticSizes)
  return (concat decs)

mkDecomposable :: TypeQ -> String -> DecsQ
mkDecomposable scalarType cType =
  [d|
    instance Decomposable $(scalarType) where
      cmFullSVD u s v m = $(blockMap [RS.r| void {
          JacobiSVD<Matrix<$type(m), Dynamic, Dynamic>> svd($mapR(m), ComputeFullU | ComputeFullV);
          $mapRW(u) = svd.matrixU();
          $mapRW(s) = svd.singularValues();
          $mapRW(v) = svd.matrixU();
        } |] (const ( scalarType, cType, Nothing )))
  |]

matekTypesTable :: M.Map C.TypeSpecifier TypeQ
matekTypesTable = M.fromList
  [ (C.TypeName "CEigenException", [t| CEigenException |]) -- needs a typedef
  ]

matekCtx :: C.Context
matekCtx = C.cppCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = matekTypesTable }