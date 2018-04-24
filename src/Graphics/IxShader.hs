{-# LANGUAGE ConstraintKinds       #-}
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
module Graphics.IxShader
  ( module Graphics.IxShader
  , KnownSymbol
  , module G
  , (&&&)
  , module Prelude
  ) where


import           Control.Arrow               ((&&&), (>>>))
import           Data.List                   (intercalate)
import           Data.Promotion.Prelude.List ((:++))
import           Data.Promotion.Prelude.Num
import           Data.Proxy
import           Data.Ratio                  (denominator, numerator)
import           Data.Singletons.TypeLits
import           Graphics.IxShader.Function   as G
import           Graphics.IxShader.IxShader   as G
import           Graphics.IxShader.Ops.Mult   as G
import           Graphics.IxShader.Qualifiers as G
import           Graphics.IxShader.Socket     as G
import           Graphics.IxShader.Swizzle    as G
import           Graphics.IxShader.Types      as G
import           Graphics.IxShader.Texture    as G
import           Graphics.IxShader.Struct     as G
import           Prelude                     hiding (Eq (..), Floating (..),
                                              Fractional (..), Num (..),
                                              Ord (..), fail, fromInteger,
                                              fromRational, length, log, mod,
                                              return, (<), (>>), (>>=))
import qualified Prelude as P

fromInteger :: Integer -> Xint
fromInteger = Xint . show

fromRational :: Rational -> Xfloat
fromRational =
  (numerator &&& denominator) >>> \(n, d) ->
    Xfloat $ show (fromIntegral n P./ fromIntegral d :: Float)

infixr 1 .=
(.=)
  :: forall a b i ctx shadertype.
     ( Socketed a, Socketed b
     , WriteTo a ~ ReadFrom b
     )
  => a -> b
  -> IxShader shadertype ctx i i ()
(.=) a b = nxt_ $ unwords [unSocket a, "=", unSocket b ++ ";"]


smoothstep :: ( Socketed a, Socketed b, Socketed c
              , Socketed (ReadFrom c)
              , ReadFrom a ~ ReadFrom b
              , ReadFrom a ~ ReadFrom c
              ) => a -> b -> c -> ReadFrom c
smoothstep = call3 "smoothstep"

step :: (Socketed a, Socketed b, Socketed c) => a -> b -> c
step = call2 "step"

mkvec2 :: Xfloat -> Xfloat -> Xvec2
mkvec2 = call2 "vec2"

mkvec3 :: Xfloat -> Xfloat -> Xfloat -> Xvec3
mkvec3 = call3 "vec3"

mkvec4 :: Xfloat -> Xfloat -> Xfloat -> Xfloat -> Xvec4
mkvec4 = call4 "vec4"

mkmat4 :: Xvec4 -> Xvec4 -> Xvec4 -> Xvec4 -> Xmat4
mkmat4 = call4 "mat4"

toInt :: Xfloat -> Xint
toInt = call "int"

toFloat :: Xint -> Xfloat
toFloat = call "float"

float :: Float -> Xfloat
float = Xfloat . show

int :: Int -> Xint
int = Xint . show

type family LengthOf a where
  LengthOf Xfloat        = Xfloat
  LengthOf Xvec2         = Xfloat
  LengthOf Xvec3         = Xfloat
  LengthOf Xvec4         = Xfloat
  LengthOf (Uniform t n) = LengthOf t
  LengthOf (In t n)      = LengthOf t
  LengthOf (Out t n)     = LengthOf t
  LengthOf (Const t)     = LengthOf t
  LengthOf a             = Error '(a, "Cannot call length on this type.")

length :: (Socketed a, Socketed (LengthOf a)) => a -> LengthOf a
length = call "length"

type family VectOf (n :: Nat) v where
  VectOf 1 Xfloat = Xfloat
  VectOf 2 Xfloat = Xvec2
  VectOf 3 Xfloat = Xvec3
  VectOf 4 Xfloat = Xvec4

  VectOf 2 Xvec2 = Xmat2
  VectOf 3 Xvec2 = Xmat3x2
  VectOf 4 Xvec2 = Xmat4x2

  VectOf 2 Xvec3 = Xmat2x3
  VectOf 3 Xvec3 = Xmat3
  VectOf 4 Xvec3 = Xmat4x3

  VectOf 2 Xvec4 = Xmat4x2
  VectOf 3 Xvec4 = Xmat4x3
  VectOf 4 Xvec4 = Xmat4

type family CompType v where
  CompType Xfloat = Xfloat
  CompType Xvec2  = Xfloat
  CompType Xvec3  = Xfloat
  CompType Xvec4  = Xfloat

  CompType Xmat2   = Xvec2
  CompType Xmat2x3 = Xvec3
  CompType Xmat2x4 = Xvec4

  CompType Xmat3x2 = Xvec2
  CompType Xmat3   = Xvec3
  CompType Xmat3x4 = Xvec4

  CompType Xmat4x2 = Xvec2
  CompType Xmat4x3 = Xvec3
  CompType Xmat4   = Xvec4

type family NumComps v where
  NumComps Xfloat = 1
  NumComps Xvec2  = 2
  NumComps Xvec3  = 3
  NumComps Xvec4  = 4

  NumComps Xmat2   = 2
  NumComps Xmat2x3 = 3
  NumComps Xmat2x4 = 4

  NumComps Xmat3x2 = 2
  NumComps Xmat3   = 3
  NumComps Xmat3x4 = 4

  NumComps Xmat4x2 = 2
  NumComps Xmat4x3 = 3
  NumComps Xmat4   = 4

type family CompCat as bs where
  CompCat as bs = VectOf (NumComps as :+ NumComps bs) (CompType as)

infixr 5 .:
(.:)
  :: forall a b.
     ( KnownTypeSymbol (CompCat (ReadFrom a) (ReadFrom b))
     , Socketed a, Socketed b, Socketed (CompCat (ReadFrom a) (ReadFrom b))
     )
  => a -> b -> CompCat (ReadFrom a) (ReadFrom b)
(.:) = call2 (typeSymbolVal $ Proxy @(CompCat (ReadFrom a) (ReadFrom b)))

type IsGLContext ctx = (HasContext ctx, KnownSymbol (GLFragName ctx))

main_
  :: forall (ctx :: GLContext)  shadertype i a.
  IxShader shadertype ctx i i a -> IxShader shadertype ctx i (i :++ '[Main]) ()
main_ f = void $ func @"main" () $ const $ do
  void f
  return nil

infixr 5 +=
(+=) :: Readable a b => a -> b -> ReadFrom a
(+=) = callInfix "+="

infixr 5 -=
(-=) :: Readable a b => a -> b -> ReadFrom a
(-=) = callInfix "-="

mod :: (Socketed a, Socketed b) => a -> b -> a
mod = call2 "mod"

at :: (Socketed a, Socketed (CompType a)) => a -> Xint -> CompType a
at v n = socket $ unSocket v ++ "[" ++ unSocket n ++ "]"

for
  :: (Socketed a, KnownTypeSymbol a)
  => (String, a)
  -> (a -> (Xbool, a))
  -> (a -> IxShader shadertype ctx i i b)
  -> IxShader shadertype ctx i i b
for (name, v) fi f = do
  let k = socket name
      (itill, iinc) = fi k
  nxt_ $ unwords [ "for ("
                 , stringDefinition k v
                 , intercalate "; " [ unSocket itill
                                    , unSocket iinc
                                    ]
                 , ")"
                 ]
  sub "{" "}" $ f k

bigfattestvertex
  :: forall (ctx :: GLContext). HasContext ctx
  => IxVertex ctx '[] '[ Uniform Xvec2 "u_resolution"
                       , Out Xvec4 "gl_Position"
                       , Function Xint "myFunc" (Xint, Xint)
                       , Main
                       ] ()
bigfattestvertex = do
  case getCtx @ctx of
    OpenGLContext -> nxt_ "#version 330 core\n"
    WebGLContext  -> nxt_ "precision highp float;\n"
  res <- uniform_
  pos <- gl_Position

  myFunc <- func (Xint "a", Xint "b") $ \(a, b) -> returnValue $ a + b

  main_ $ do
    unless (1 == 1) $ do
      _ <- def "xxx" 1
      _ <- def "yyy" $ length pos
      return ()
    fa   <- def "a" 1.0
    fb   <- def "b" 2.0
    v4   <- def "v4" $ (fa - fb) .: fb .: mkvec2 3.0 4.0
    v4   .= mkvec2 3.0 4.0 .: fa .: fb
    v4   .= mkvec4 1.0 2.0 3.0 4.0
    xwhy <- def "exwhy" $ fa .: fb
    x xwhy .= 0.0
    xwhy `at` 0 .= 2.0
    xwhy `at` 1 .= 5.0
    m4   <- def "m4" $ mkmat4 v4 v4 v4 v4
    m4 `at` 2 .= mkvec4 0.0 0.0 1.0 0.0
    pa   <- def "paramA" 0.0
    pb   <- def "paramB" 1.0
    c    <- def "c" $ myFunc (toInt pa, toInt pb)
    c    .= 5
    c    .= toInt (x res)
    x res .= 0.0
    for ("i", 0) ((< 5) &&& (+= 1)) $ \i -> pos .= mkvec4 (toFloat i) 2.0 3.0 4.0
