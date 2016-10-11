{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Matek.Scalars() where

import           Data.Int
import           Foreign.C
import qualified Language.C.Inline.Cpp as C

import           Matek.Inline
import           Matek.Types

C.context matekCtx
C.include "matek_exceptions.h"

C.include "Eigen/Dense"

C.using "namespace Eigen"

-- Generate specializations for the scalar types.
instance Scalar Double where
  type CScalar Double = CDouble
mkSpecs [t|Double|] "double"
  [ ( 2, 1 )
  , ( 3, 1 )
  , ( 4, 1 )
  ]

instance Scalar Float where
  type CScalar Float = CFloat
mkSpecs [t|Float|] "float"
  [ ( 2, 1 )
  , ( 3, 1 )
  , ( 4, 1 )
  ]

instance Scalar Int64 where
  type CScalar Int64 = Int64
mkSpecs [t|Int64|] "int64_t" []

instance Scalar Int32 where
  type CScalar Int32 = Int32
mkSpecs [t|Int32|] "int32_t" []

mkDecomposable [t|Double|] "double"
mkDecomposable [t|Float|] "float"

instance ShowScalar Double where
  showScalar = showRealFloat
instance ShowScalar Float where
  showScalar = showRealFloat
instance ShowScalar Int64 where
  showScalar = show
instance ShowScalar Int32 where
  showScalar = show