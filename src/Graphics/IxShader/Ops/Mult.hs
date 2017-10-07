{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.IxShader.Ops.Mult where

import           Prelude                 hiding (Read, return, (>>), (>>=))

import           Graphics.IxShader.Socket
import           Graphics.IxShader.Types
import           Graphics.IxShader.Qualifiers


type family MatFromColRow a b where
  MatFromColRow 2 2 = Xmat2
  MatFromColRow 2 3 = Xmat2x3
  MatFromColRow 2 4 = Xmat2x4
  MatFromColRow 3 3 = Xmat3
  MatFromColRow 3 2 = Xmat3x2
  MatFromColRow 3 4 = Xmat3x4
  MatFromColRow 4 4 = Xmat4
  MatFromColRow 4 2 = Xmat4x2
  MatFromColRow 4 3 = Xmat4x3

type family MatCol a where
  MatCol Xvec2   = 2
  MatCol Xvec3   = 3
  MatCol Xvec4   = 4
  MatCol Xmat2   = 2
  MatCol Xmat2x3 = 2
  MatCol Xmat2x4 = 2
  MatCol Xmat3   = 3
  MatCol Xmat3x2 = 3
  MatCol Xmat3x4 = 3
  MatCol Xmat4   = 4
  MatCol Xmat4x2 = 4
  MatCol Xmat4x3 = 4

type family MatRow a where
  MatRow Xvec2   = 1
  MatRow Xvec3   = 1
  MatRow Xvec4   = 1
  MatRow Xmat2   = 2
  MatRow Xmat2x3 = 2
  MatRow Xmat2x4 = 2
  MatRow Xmat3   = 3
  MatRow Xmat3x2 = 3
  MatRow Xmat3x4 = 3
  MatRow Xmat4   = 4
  MatRow Xmat4x2 = 4
  MatRow Xmat4x3 = 4

type family FromMatColRow ac ar at bc br bt where
  FromMatColRow _ 1 a _ _ b = a
  FromMatColRow _ _ a _ 1 b = b
  FromMatColRow c _ _ _ r _ = MatFromColRow c r

type family Multiply a b where
  Multiply Xfloat Xvec2   = Xvec2
  Multiply Xfloat Xvec3   = Xvec3
  Multiply Xfloat Xvec4   = Xvec4
  Multiply Xfloat Xmat2   = Xmat2
  Multiply Xfloat Xmat2x3 = Xmat2x3
  Multiply Xfloat Xmat2x4 = Xmat2x4
  Multiply Xfloat Xmat3x2 = Xmat3x2
  Multiply Xfloat Xmat3   = Xmat3
  Multiply Xfloat Xmat3x4 = Xmat3x4
  Multiply Xfloat Xmat4x2 = Xmat4x2
  Multiply Xfloat Xmat4x3 = Xmat4x3
  Multiply Xfloat Xmat4   = Xmat4

  Multiply Xvec2   Xfloat = Xvec2
  Multiply Xvec3   Xfloat = Xvec3
  Multiply Xvec4   Xfloat = Xvec4
  Multiply Xmat2   Xfloat = Xmat2
  Multiply Xmat2x3 Xfloat = Xmat2x3
  Multiply Xmat2x4 Xfloat = Xmat2x4
  Multiply Xmat3x2 Xfloat = Xmat3x2
  Multiply Xmat3   Xfloat = Xmat3
  Multiply Xmat3x4 Xfloat = Xmat3x4
  Multiply Xmat4x2 Xfloat = Xmat4x2
  Multiply Xmat4x3 Xfloat = Xmat4x3
  Multiply Xmat4   Xfloat = Xmat4

  Multiply Xuint Xuvec2   = Xuvec2
  Multiply Xuint Xuvec3   = Xuvec3
  Multiply Xuint Xuvec4   = Xuvec4
  Multiply a     Xuint    = Multiply Xuint a

  Multiply Xint Xivec2   = Xivec2
  Multiply Xint Xivec3   = Xivec3
  Multiply Xint Xivec4   = Xivec4
  Multiply a     Xint    = Multiply Xint a

  Multiply a b = FromMatColRow (MatCol a) (MatRow a) a (MatCol b) (MatRow b) b

-- | Multiply two sockets.
infixl 7 .*
(.*)
  :: forall a b. (Socketed a, Socketed b, Socketed (Multiply (ReadFrom a) (ReadFrom b)))
  => a
  -> b
  -> Multiply (ReadFrom a) (ReadFrom b)
(.*) = callInfix "*"
