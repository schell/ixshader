{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.IxShader.Swizzle
  ( Swizzled
  , swizzle
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  ) where

import           Data.Proxy                  (Proxy (..))
import           Data.Singletons.TypeLits
import           Prelude                     hiding (Read, return, (>>), (>>=))


import           Graphics.IxShader.Socket     as G
import           Graphics.IxShader.Qualifiers     as G
import           Graphics.IxShader.Types      as G

type family Swizzled a b where
  Swizzled 1 Xvec2 = Xfloat
  Swizzled 1 Xvec3 = Xfloat
  Swizzled 1 Xvec4 = Xfloat
  Swizzled 2 Xvec2 = Xvec2
  Swizzled 2 Xvec3 = Xvec2
  Swizzled 2 Xvec4 = Xvec2
  Swizzled 3 Xvec2 = Error "Swizzled error: vector field selection out of range"
  Swizzled 3 Xvec3 = Xvec3
  Swizzled 3 Xvec4 = Xvec3
  Swizzled 4 Xvec2 = Error "Swizzled error: vector field selection out of range"
  Swizzled 4 Xvec3 = Error "Swizzled error: vector field selection out of range"
  Swizzled 4 Xvec4 = Xvec4

type SwizzleRead a n =
  (Socketed a, Socketed (ReadFrom a), Socketed (Swizzled n (ReadFrom a)))

swizzle
  :: forall (n :: Nat) a.
     ( SwizzleRead a n
     , KnownNat n
     )
  => String
  -> a
  -> Swizzled n (ReadFrom a)
swizzle s a = socket $ concat ["("
                              , unSocket a
                              , ")." ++ take (fromIntegral $ natVal $ Proxy @n) s
                              ]

x :: forall a.
     SwizzleRead a 1
  => a
  -> Swizzled 1 (ReadFrom a)
x = swizzle @1 "x"

y :: forall a.
     SwizzleRead a 1
  => a
  -> Swizzled 1 (ReadFrom a)
y = swizzle @1 "y"

z :: forall a.
     SwizzleRead a 1
  => a
  -> Swizzled 1 (ReadFrom a)
z = swizzle @1 "z"

xy :: forall a.
     SwizzleRead a 2
  => a
  -> Swizzled 2 (ReadFrom a)
xy = swizzle @2 "xy"

xz :: forall a.
     SwizzleRead a 2
  => a
  -> Swizzled 2 (ReadFrom a)
xz = swizzle @2 "xz"

yz :: forall a.
     SwizzleRead a 2
  => a
  -> Swizzled 2 (ReadFrom a)
yz = swizzle @2 "yz"

xyz :: forall a.
     SwizzleRead a 3
  => a
  -> Swizzled 3 (ReadFrom a)
xyz = swizzle @3 "xyz"
