{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Matek.Scalars() where

import           Data.Int
import           Foreign.C
import qualified Language.C.Inline.Cpp as C

import           Matek.Inline
import           Matek.Types

C.context C.cppCtx
C.include "Eigen/Dense"

-- Generate specializations for the scalar types.
mkScalar ''Double ''CDouble "double"
mkScalar ''Float ''CFloat "float"
mkScalar ''Int64 ''Int64 "int64_t"
mkScalar ''Int32 ''Int32 "int32_t"

instance ShowScalar Double where
  showScalar = showRealFloat
instance ShowScalar Float where
  showScalar = showRealFloat
instance ShowScalar Int64 where
  showScalar = show
instance ShowScalar Int32 where
  showScalar = show