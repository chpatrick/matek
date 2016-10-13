{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Matek.Inline
  ( mkScalar
  , mkDecomposable
  , blockMap
  , matekCtx
  ) where

import qualified Data.Map as M
import           Data.Monoid
import           Data.Traversable
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

-- like C.block but $map(r) (where r is a CMatrix a) gets replaced with an Eigen Map with the given element type
blockMap :: String -> (String -> ( Name, String )) -> Q Exp
blockMap cBlock mapTypeOf = do
  let cBlockTokens = tokenize cBlock
  blockResults <- for cBlockTokens $ \case
    CharToken c -> return ( [ c ], [] )
    MapQuoteToken cmName acc -> do
      let ptrName = mkName (cmName ++ "_cmPtr_inline")
      let rowsName = mkName (cmName ++ "_cmRows_inline")
      let colsName = mkName (cmName ++ "_cmCols_inline")
      let ( mapType, cType ) = mapTypeOf cmName
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
      return ( replacement, [ ( ptrName, rowsName, colsName, mkName cmName, mapType, acc ) ] )
    TypeQuoteToken cmName -> do
      let ( _ , cType ) = mapTypeOf cmName
      return ( cType, [] )
  let ( translatedCBlockStrings, cmBinds ) = unzip blockResults
  let translatedCBlock = concat translatedCBlockStrings
  let bindCMs =
        [ letS
          [ valD
            (recP 'CMatrix
              [ fieldPat 'cmData (varP ptrName)
              , fieldPat 'cmRows (varP rowsName)
              , fieldPat 'cmCols (varP colsName)
              ])
            (normalB [e| $(varE cmName) :: CMatrix $(accType) $(mapTypeType) |] )
            []
          ]
        | ( ptrName, rowsName, colsName, cmName, mapType, acc ) <- concat cmBinds
        , let mapTypeType = conT mapType
        , let accType = case acc of
                R -> [t|'R|]
                RW -> [t|'RW|]
        ]
  doE (bindCMs ++ [ noBindS (quoteExp CU.block translatedCBlock) ])

mkScalar :: Name -> Name -> String -> DecsQ
mkScalar scalarName cScalarName cType =
  [d|
    instance Scalar $(scalar) where
      type CScalar $(scalar) = $(cScalar)

      cmMul r x y = $(blockMap [RS.r| void {
          $mapRW(r) = $mapR(x) * $mapR(y);
        } |] (const ( scalarName, cType )))
      cmCopyBlock r dstRow dstCol x = $(blockMap [RS.r| void {
          auto src = $mapR(x);
          $mapRW(r).block($(size_t dstRow), $(size_t dstCol), src.rows(), src.cols()) = src;
        } |] (const ( scalarName, cType )))
  |]
    where
      scalar = conT scalarName
      cScalar = conT cScalarName

mkDecomposable :: Name -> String -> DecsQ
mkDecomposable scalarName cType =
  [d|
    instance Decomposable $(conT scalarName) where
      cmFullSVD u s v m = $(blockMap [RS.r| void {
          JacobiSVD<Matrix<$type(m), Dynamic, Dynamic>> svd($mapR(m), ComputeFullU | ComputeFullV);
          $mapRW(u) = svd.matrixU();
          $mapRW(s) = svd.singularValues();
          $mapRW(v) = svd.matrixU();
        } |] (const ( scalarName, cType )))
  |]

matekTypesTable :: M.Map C.TypeSpecifier TypeQ
matekTypesTable = M.fromList
  [ (C.TypeName "CEigenException", [t| CEigenException |]) -- needs a typedef
  ]

matekCtx :: C.Context
matekCtx = C.cppCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = matekTypesTable }