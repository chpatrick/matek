{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Matek.Inline
  ( mkScalar
  , blockMap
  ) where

import           Data.Traversable
import qualified Language.C.Inline.Unsafe as CU
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token

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

-- like C.block but $map(r) (where r is a CM a) gets replaced with an Eigen Map with the given element type
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
            [ "Eigen::Map<"
            , case acc of { RW -> ""; R -> "const " }
            , "Eigen::Matrix<"
            , cType
            , ", Eigen::Dynamic, Eigen::Dynamic>>($("
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
            (recP 'CM
              [ fieldPat 'cmData (varP ptrName)
              , fieldPat 'cmRows (varP rowsName)
              , fieldPat 'cmCols (varP colsName)
              ])
            (normalB [e| $(varE cmName) :: CM $(accType) $(mapTypeType) |] )
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

      cmPlus r x y = $(blockMap "void { $mapRW(r) = $mapR(x) + $mapR(y); }" (const ( scalarName, cType )))
      cmMinus r x y = $(blockMap "void { $mapRW(r) = $mapR(x) - $mapR(y); }" (const ( scalarName, cType )))
      cmMul r x y = $(blockMap "void { $mapRW(r) = $mapR(x) * $mapR(y); }" (const ( scalarName, cType )))
      cmTranspose r x = $(blockMap "void { $mapRW(r) = $mapR(x).transpose(); }" (const ( scalarName, cType )))
      cmAbs r x = $(blockMap "void { $mapRW(r).array() = $mapR(x).array().abs(); }" (const ( scalarName, cType )))
      cmMap f r x = $(blockMap "void { $mapRW(r) = $mapR(x).unaryExpr(std::ptr_fun($($type(x) (*f)($type(x)) ))); }" (const ( scalarName, cType )))
      cmSignum r x = $(blockMap "void { $mapRW(r) = $mapR(x).unaryExpr([]($type(x) n) { return ((n > 0 ? 1 : (n == 0 ? 0 : -1))); }); }" (const ( scalarName, cType )))
      cmScale k r x = $(blockMap "void { $mapRW(r) = $mapR(x) * $($type(x) k); }" (const ( scalarName, cType )))
  |]
    where
      scalar = conT scalarName
      cScalar = conT cScalarName
