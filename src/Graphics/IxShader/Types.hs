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
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.IxShader.Types
  ( module Graphics.IxShader.Types
  , module G
  ) where


import           Prelude                      hiding (Ord (..), Read, return,
                                               (>>), (>>=))

import           Graphics.IxShader.Socket      as G
import           Graphics.IxShader.Types.Xbool as G
import           Graphics.IxShader.Function.ToParams

--------------------------------------------------------------------------------
-- int, uint, float
--------------------------------------------------------------------------------
newtype Xvoid = Xvoid { unXvoid :: String }
instance Socketed Xvoid where
  unSocket = unXvoid
  socket = Xvoid
instance KnownTypeSymbol Xvoid where
  typeSymbolVal _ = "void"

nil :: Xvoid
nil = Xvoid ""

--------------------------------------------------------------------------------
-- int, uint, float
--------------------------------------------------------------------------------
newtype Xint = Xint { unXint :: String }
$(genKnownTypeSymbol [t|Xint|] [e|"int"|])
$(genSocketed        [t|Xint|] [e|unXint|] [e|Xint|])
$(genToParams        [t|Xint|])

--int :: Int -> Xint
--int = Xint . show

newtype Xuint = Xuint { unXuint :: String }
$(genKnownTypeSymbol [t|Xuint|] [e|"uint"|])
$(genSocketed        [t|Xuint|] [e|unXuint|] [e|Xuint|])
$(genToParams        [t|Xuint|])

--uint :: Word -> Xuint
--uint = Xuint . show

newtype Xfloat = Xfloat { unXfloat :: String }
$(genKnownTypeSymbol [t|Xfloat|] [e|"float"|])
$(genSocketed        [t|Xfloat|] [e|unXfloat|] [e|Xfloat|])
$(genToParams        [t|Xfloat|])


pi :: Socketed a => a
pi = socket $ show (Prelude.pi :: Float)

--float :: Float -> Xfloat
--float = Xfloat . show

--------------------------------------------------------------------------------
-- vec[2,3,4]
--------------------------------------------------------------------------------
newtype Xvec2 = Xvec2 { unXvec2 :: String }
$(genKnownTypeSymbol [t|Xvec2|] [e|"vec2"|])
$(genSocketed        [t|Xvec2|] [e|unXvec2|] [e|Xvec2|])
$(genToParams        [t|Xvec2|])

--vec2 :: Float -> Float -> Xvec2
--vec2

newtype Xvec3 = Xvec3 { unXvec3 :: String }
$(genKnownTypeSymbol [t|Xvec3|] [e|"vec3"|])
$(genSocketed        [t|Xvec3|] [e|unXvec3|] [e|Xvec3|])
$(genToParams        [t|Xvec3|])

newtype Xvec4 = Xvec4 { unXvec4 :: String }
$(genKnownTypeSymbol [t|Xvec4|] [e|"vec4"|])
$(genSocketed        [t|Xvec4|] [e|unXvec4|] [e|Xvec4|])
$(genToParams        [t|Xvec4|])


--------------------------------------------------------------------------------
-- bvec[2,3,4]
--------------------------------------------------------------------------------
newtype Xbvec2 = Xbvec2 { unXbvec2 :: String }
$(genKnownTypeSymbol [t|Xbvec2|] [e|"bvec2"|])
$(genSocketed        [t|Xbvec2|] [e|unXbvec2|] [e|Xbvec2|])
$(genToParams        [t|Xbvec2|])

newtype Xbvec3 = Xbvec3 { unXbvec3 :: String }
$(genKnownTypeSymbol [t|Xbvec3|] [e|"bvec3"|])
$(genSocketed        [t|Xbvec3|] [e|unXbvec3|] [e|Xbvec3|])
$(genToParams        [t|Xbvec3|])

newtype Xbvec4 = Xbvec4 { unXbvec4 :: String }
$(genKnownTypeSymbol [t|Xbvec4|] [e|"bvec4"|])
$(genSocketed        [t|Xbvec4|] [e|unXbvec4|] [e|Xbvec4|])
$(genToParams        [t|Xbvec4|])


--------------------------------------------------------------------------------
-- ivec[2,3,4]
--------------------------------------------------------------------------------
newtype Xivec2 = Xivec2 { unXivec2 :: String }
$(genKnownTypeSymbol [t|Xivec2|] [e|"ivec2"|])
$(genSocketed        [t|Xivec2|] [e|unXivec2|] [e|Xivec2|])
$(genToParams        [t|Xivec2|])

newtype Xivec3 = Xivec3 { unXivec3 :: String }
$(genKnownTypeSymbol [t|Xivec3|] [e|"ivec3"|])
$(genSocketed        [t|Xivec3|] [e|unXivec3|] [e|Xivec3|])
$(genToParams        [t|Xivec3|])

newtype Xivec4 = Xivec4 { unXivec4 :: String }
$(genKnownTypeSymbol [t|Xivec4|] [e|"ivec4"|])
$(genSocketed        [t|Xivec4|] [e|unXivec4|] [e|Xivec4|])
$(genToParams        [t|Xivec4|])


--------------------------------------------------------------------------------
-- uvec[2,3,4]
--------------------------------------------------------------------------------
newtype Xuvec2 = Xuvec2 { unXuvec2 :: String }
$(genKnownTypeSymbol [t|Xuvec2|] [e|"uvec2"|])
$(genSocketed        [t|Xuvec2|] [e|unXuvec2|] [e|Xuvec2|])
$(genToParams        [t|Xuvec2|])

newtype Xuvec3 = Xuvec3 { unXuvec3 :: String }
$(genKnownTypeSymbol [t|Xuvec3|] [e|"uvec3"|])
$(genSocketed        [t|Xuvec3|] [e|unXuvec3|] [e|Xuvec3|])
$(genToParams        [t|Xuvec3|])

newtype Xuvec4 = Xuvec4 { unXuvec4 :: String }
$(genKnownTypeSymbol [t|Xuvec4|] [e|"uvec4"|])
$(genSocketed        [t|Xuvec4|] [e|unXuvec4|] [e|Xuvec4|])
$(genToParams        [t|Xuvec4|])


--------------------------------------------------------------------------------
-- mat2x[2,3,4]
--------------------------------------------------------------------------------
newtype Xmat2 = Xmat2 { unXmat2 :: String }
$(genKnownTypeSymbol [t|Xmat2|] [e|"mat2"|])
$(genSocketed        [t|Xmat2|] [e|unXmat2|] [e|Xmat2|])
$(genToParams        [t|Xmat2|])
type Xmat2x2 = Xmat2

newtype Xmat2x3 = Xmat2x3 { unXmat2x3 :: String }
$(genKnownTypeSymbol [t|Xmat2x3|] [e|"mat2x3"|])
$(genSocketed        [t|Xmat2x3|] [e|unXmat2x3|] [e|Xmat2x3|])
$(genToParams        [t|Xmat2x3|])

newtype Xmat2x4 = Xmat2x4 { unXmat2x4 :: String }
$(genKnownTypeSymbol [t|Xmat2x4|] [e|"mat2x4"|])
$(genSocketed        [t|Xmat2x4|] [e|unXmat2x4|] [e|Xmat2x4|])
$(genToParams        [t|Xmat2x4|])


--------------------------------------------------------------------------------
-- mat3x[2,3,4]
--------------------------------------------------------------------------------
newtype Xmat3x2 = Xmat3x2 { unXmat3x2 :: String }
$(genKnownTypeSymbol [t|Xmat3x2|] [e|"mat3x2"|])
$(genSocketed        [t|Xmat3x2|] [e|unXmat3x2|] [e|Xmat3x2|])
$(genToParams        [t|Xmat3x2|])

newtype Xmat3 = Xmat3 { unXmat3 :: String }
$(genKnownTypeSymbol [t|Xmat3|] [e|"mat3"|])
$(genSocketed        [t|Xmat3|] [e|unXmat3|] [e|Xmat3|])
$(genToParams        [t|Xmat3|])
type Xmat3x3 = Xmat3

newtype Xmat3x4 = Xmat3x4 { unXmat3x4 :: String }
$(genKnownTypeSymbol [t|Xmat3x4|] [e|"mat3x4"|])
$(genSocketed        [t|Xmat3x4|] [e|unXmat3x4|] [e|Xmat3x4|])
$(genToParams        [t|Xmat3x4|])

--------------------------------------------------------------------------------
-- mat4x[2,3,4]
--------------------------------------------------------------------------------
newtype Xmat4x2 = Xmat4x2 { unXmat4x2 :: String }
$(genKnownTypeSymbol [t|Xmat4x2|] [e|"mat4x2"|])
$(genSocketed        [t|Xmat4x2|] [e|unXmat4x2|] [e|Xmat4x2|])
$(genToParams        [t|Xmat4x2|])

newtype Xmat4x3 = Xmat4x3 { unXmat4x3 :: String }
$(genKnownTypeSymbol [t|Xmat4x3|] [e|"mat4x3"|])
$(genSocketed        [t|Xmat4x3|] [e|unXmat4x3|] [e|Xmat4x3|])
$(genToParams        [t|Xmat4x3|])

newtype Xmat4 = Xmat4 { unXmat4 :: String }
$(genKnownTypeSymbol [t|Xmat4|] [e|"mat4"|])
$(genSocketed        [t|Xmat4|] [e|unXmat4|] [e|Xmat4|])
$(genToParams        [t|Xmat4|])
type Xmat4x4 = Xmat4

----------------------------------------------------------------------
-- Sampler Types (Opaque)
----------------------------------------------------------------------
newtype Xsampler1D = Xsampler1D { unXsampler1D :: String }
$(genKnownTypeSymbol [t|Xsampler1D|] [e|"sampler1D"|])
$(genSocketed        [t|Xsampler1D|] [e|unXsampler1D|] [e|Xsampler1D|])
$(genToParams        [t|Xsampler1D|])

newtype Xsampler2D = Xsampler2D { unXsampler2D :: String }
$(genKnownTypeSymbol [t|Xsampler2D|] [e|"sampler2D"|])
$(genSocketed        [t|Xsampler2D|] [e|unXsampler2D|] [e|Xsampler2D|])
$(genToParams        [t|Xsampler2D|])

newtype Xsampler3D = Xsampler3D { unXsampler3D :: String }
$(genKnownTypeSymbol [t|Xsampler3D|] [e|"sampler3D"|])
$(genSocketed        [t|Xsampler3D|] [e|unXsampler3D|] [e|Xsampler3D|])
$(genToParams        [t|Xsampler3D|])

newtype XsamplerCube = XsamplerCube { unXsamplerCube :: String }
$(genKnownTypeSymbol [t|XsamplerCube|] [e|"samplerCube"|])
$(genSocketed        [t|XsamplerCube|] [e|unXsamplerCube|] [e|XsamplerCube|])
$(genToParams        [t|XsamplerCube|])

newtype XsamplerCubeShadow = XsamplerCubeShadow { unXsamplerCubeShadow :: String }
$(genKnownTypeSymbol [t|XsamplerCubeShadow|] [e|"samplerCubeShadow"|])
$(genSocketed        [t|XsamplerCubeShadow|] [e|unXsamplerCubeShadow|] [e|XsamplerCubeShadow|])
$(genToParams        [t|XsamplerCubeShadow|])

newtype Xsampler2DRect = Xsampler2DRect { unXsampler2DRect :: String }
$(genKnownTypeSymbol [t|Xsampler2DRect|] [e|"sampler2DRect"|])
$(genSocketed        [t|Xsampler2DRect|] [e|unXsampler2DRect|] [e|Xsampler2DRect|])
$(genToParams        [t|Xsampler2DRect|])

newtype Xsampler1DShadow = Xsampler1DShadow { unXsampler1DShadow :: String }
$(genKnownTypeSymbol [t|Xsampler1DShadow|] [e|"sampler1DShadow"|])
$(genSocketed        [t|Xsampler1DShadow|] [e|unXsampler1DShadow|] [e|Xsampler1DShadow|])
$(genToParams        [t|Xsampler1DShadow|])

newtype Xsampler2DShadow = Xsampler2DShadow { unXsampler2DShadow :: String }
$(genKnownTypeSymbol [t|Xsampler2DShadow|] [e|"sampler2DShadow"|])
$(genSocketed        [t|Xsampler2DShadow|] [e|unXsampler2DShadow|] [e|Xsampler2DShadow|])
$(genToParams        [t|Xsampler2DShadow|])

newtype Xsampler2DRectShadow = Xsampler2DRectShadow { unXsampler2DRectShadow :: String }
$(genKnownTypeSymbol [t|Xsampler2DRectShadow|] [e|"sampler2DRectShadow"|])
$(genSocketed        [t|Xsampler2DRectShadow|] [e|unXsampler2DRectShadow|] [e|Xsampler2DRectShadow|])
$(genToParams        [t|Xsampler2DRectShadow|])

newtype Xsampler1DArray = Xsampler1DArray { unXsampler1DArray :: String }
$(genKnownTypeSymbol [t|Xsampler1DArray|] [e|"sampler1DArray"|])
$(genSocketed        [t|Xsampler1DArray|] [e|unXsampler1DArray|] [e|Xsampler1DArray|])
$(genToParams        [t|Xsampler1DArray|])

newtype Xsampler2DArray = Xsampler2DArray { unXsampler2DArray :: String }
$(genKnownTypeSymbol [t|Xsampler2DArray|] [e|"sampler2DArray"|])
$(genSocketed        [t|Xsampler2DArray|] [e|unXsampler2DArray|] [e|Xsampler2DArray|])
$(genToParams        [t|Xsampler2DArray|])

newtype Xsampler1DArrayShadow = Xsampler1DArrayShadow { unXsampler1DArrayShadow :: String }
$(genKnownTypeSymbol [t|Xsampler1DArrayShadow|] [e|"sampler1DArrayShadow"|])
$(genSocketed        [t|Xsampler1DArrayShadow|] [e|unXsampler1DArrayShadow|] [e|Xsampler1DArrayShadow|])
$(genToParams        [t|Xsampler1DArrayShadow|])

newtype Xsampler2DArrayShadow = Xsampler2DArrayShadow { unXsampler2DArrayShadow :: String }
$(genKnownTypeSymbol [t|Xsampler2DArrayShadow|] [e|"sampler2DArrayShadow"|])
$(genSocketed        [t|Xsampler2DArrayShadow|] [e|unXsampler2DArrayShadow|] [e|Xsampler2DArrayShadow|])
$(genToParams        [t|Xsampler2DArrayShadow|])

newtype XsamplerBuffer = XsamplerBuffer { unXsamplerBuffer :: String }
$(genKnownTypeSymbol [t|XsamplerBuffer|] [e|"samplerBuffer"|])
$(genSocketed        [t|XsamplerBuffer|] [e|unXsamplerBuffer|] [e|XsamplerBuffer|])
$(genToParams        [t|XsamplerBuffer|])

newtype Xsampler2DMS = Xsampler2DMS { unXsampler2DMS :: String }
$(genKnownTypeSymbol [t|Xsampler2DMS|] [e|"sampler2DMS"|])
$(genSocketed        [t|Xsampler2DMS|] [e|unXsampler2DMS|] [e|Xsampler2DMS|])
$(genToParams        [t|Xsampler2DMS|])

newtype Xsampler2DMSArray = Xsampler2DMSArray { unXsampler2DMSArray :: String }
$(genKnownTypeSymbol [t|Xsampler2DMSArray|] [e|"sampler2DMSArray"|])
$(genSocketed        [t|Xsampler2DMSArray|] [e|unXsampler2DMSArray|] [e|Xsampler2DMSArray|])
$(genToParams        [t|Xsampler2DMSArray|])

--------------------------------------------------------------------------------
-- Unsigned sampler types
--------------------------------------------------------------------------------
newtype Xusampler1D = Xusampler1D { unXusampler1D :: String }
$(genKnownTypeSymbol [t|Xusampler1D|] [e|"usampler1D"|])
$(genSocketed        [t|Xusampler1D|] [e|unXusampler1D|] [e|Xusampler1D|])
$(genToParams        [t|Xusampler1D|])

newtype Xusampler2D = Xusampler2D { unXusampler2D :: String }
$(genKnownTypeSymbol [t|Xusampler2D|] [e|"usampler2D"|])
$(genSocketed        [t|Xusampler2D|] [e|unXusampler2D|] [e|Xusampler2D|])
$(genToParams        [t|Xusampler2D|])

newtype Xusampler3D = Xusampler3D { unXusampler3D :: String }
$(genKnownTypeSymbol [t|Xusampler3D|] [e|"usampler3D"|])
$(genSocketed        [t|Xusampler3D|] [e|unXusampler3D|] [e|Xusampler3D|])
$(genToParams        [t|Xusampler3D|])

newtype XusamplerCube = XusamplerCube { unXusamplerCube :: String }
$(genKnownTypeSymbol [t|XusamplerCube|] [e|"usamplerCube"|])
$(genSocketed        [t|XusamplerCube|] [e|unXusamplerCube|] [e|XusamplerCube|])
$(genToParams        [t|XusamplerCube|])

newtype Xusampler2DRect = Xusampler2DRect { unXusampler2DRect :: String }
$(genKnownTypeSymbol [t|Xusampler2DRect|] [e|"usampler2DRect"|])
$(genSocketed        [t|Xusampler2DRect|] [e|unXusampler2DRect|] [e|Xusampler2DRect|])
$(genToParams        [t|Xusampler2DRect|])

newtype Xusampler1DArray = Xusampler1DArray { unXusampler1DArray :: String }
$(genKnownTypeSymbol [t|Xusampler1DArray|] [e|"usampler1DArray"|])
$(genSocketed        [t|Xusampler1DArray|] [e|unXusampler1DArray|] [e|Xusampler1DArray|])
$(genToParams        [t|Xusampler1DArray|])

newtype Xusampler2DArray = Xusampler2DArray { unXusampler2DArray :: String }
$(genKnownTypeSymbol [t|Xusampler2DArray|] [e|"usampler2DArray"|])
$(genSocketed        [t|Xusampler2DArray|] [e|unXusampler2DArray|] [e|Xusampler2DArray|])
$(genToParams        [t|Xusampler2DArray|])

newtype XusamplerBuffer = XusamplerBuffer { unXusamplerBuffer :: String }
$(genKnownTypeSymbol [t|XusamplerBuffer|] [e|"usamplerBuffer"|])
$(genSocketed        [t|XusamplerBuffer|] [e|unXusamplerBuffer|] [e|XusamplerBuffer|])
$(genToParams        [t|XusamplerBuffer|])

newtype Xusampler2DMS = Xusampler2DMS { unXusampler2DMS :: String }
$(genKnownTypeSymbol [t|Xusampler2DMS|] [e|"usampler2DMS"|])
$(genSocketed        [t|Xusampler2DMS|] [e|unXusampler2DMS|] [e|Xusampler2DMS|])
$(genToParams        [t|Xusampler2DMS|])

newtype Xusampler2DMSArray = Xusampler2DMSArray { unXusampler2DMSArray :: String }
$(genKnownTypeSymbol [t|Xusampler2DMSArray|] [e|"usampler2DMSArray"|])
$(genSocketed        [t|Xusampler2DMSArray|] [e|unXusampler2DMSArray|] [e|Xusampler2DMSArray|])
$(genToParams        [t|Xusampler2DMSArray|])

--------------------------------------------------------------------------------
-- Integer sampler types
--------------------------------------------------------------------------------
newtype Xisampler1D = Xisampler1D { unXisampler1D :: String }
$(genKnownTypeSymbol [t|Xisampler1D|] [e|"isampler1D"|])
$(genSocketed        [t|Xisampler1D|] [e|unXisampler1D|] [e|Xisampler1D|])
$(genToParams        [t|Xisampler1D|])

newtype Xisampler2D = Xisampler2D { unXisampler2D :: String }
$(genKnownTypeSymbol [t|Xisampler2D|] [e|"isampler2D"|])
$(genSocketed        [t|Xisampler2D|] [e|unXisampler2D|] [e|Xisampler2D|])
$(genToParams        [t|Xisampler2D|])

newtype Xisampler3D = Xisampler3D { unXisampler3D :: String }
$(genKnownTypeSymbol [t|Xisampler3D|] [e|"isampler3D"|])
$(genSocketed        [t|Xisampler3D|] [e|unXisampler3D|] [e|Xisampler3D|])
$(genToParams        [t|Xisampler3D|])

newtype XisamplerCube = XisamplerCube { unXisamplerCube :: String }
$(genKnownTypeSymbol [t|XisamplerCube|] [e|"isamplerCube"|])
$(genSocketed        [t|XisamplerCube|] [e|unXisamplerCube|] [e|XisamplerCube|])
$(genToParams        [t|XisamplerCube|])

newtype Xisampler2DRect = Xisampler2DRect { unXisampler2DRect :: String }
$(genKnownTypeSymbol [t|Xisampler2DRect|] [e|"isampler2DRect"|])
$(genSocketed        [t|Xisampler2DRect|] [e|unXisampler2DRect|] [e|Xisampler2DRect|])
$(genToParams        [t|Xisampler2DRect|])

newtype Xisampler1DArray = Xisampler1DArray { unXisampler1DArray :: String }
$(genKnownTypeSymbol [t|Xisampler1DArray|] [e|"isampler1DArray"|])
$(genSocketed        [t|Xisampler1DArray|] [e|unXisampler1DArray|] [e|Xisampler1DArray|])
$(genToParams        [t|Xisampler1DArray|])

newtype Xisampler2DArray = Xisampler2DArray { unXisampler2DArray :: String }
$(genKnownTypeSymbol [t|Xisampler2DArray|] [e|"isampler2DArray"|])
$(genSocketed        [t|Xisampler2DArray|] [e|unXisampler2DArray|] [e|Xisampler2DArray|])
$(genToParams        [t|Xisampler2DArray|])

newtype XisamplerBuffer = XisamplerBuffer { unXisamplerBuffer :: String }
$(genKnownTypeSymbol [t|XisamplerBuffer|] [e|"isamplerBuffer"|])
$(genSocketed        [t|XisamplerBuffer|] [e|unXisamplerBuffer|] [e|XisamplerBuffer|])
$(genToParams        [t|XisamplerBuffer|])

newtype Xisampler2DMS = Xisampler2DMS { unXisampler2DMS :: String }
$(genKnownTypeSymbol [t|Xisampler2DMS|] [e|"isampler2DMS"|])
$(genSocketed        [t|Xisampler2DMS|] [e|unXisampler2DMS|] [e|Xisampler2DMS|])
$(genToParams        [t|Xisampler2DMS|])

newtype Xisampler2DMSArray = Xisampler2DMSArray { unXisampler2DMSArray :: String }
$(genKnownTypeSymbol [t|Xisampler2DMSArray|] [e|"isampler2DMSArray"|])
$(genSocketed        [t|Xisampler2DMSArray|] [e|unXisampler2DMSArray|] [e|Xisampler2DMSArray|])
$(genToParams        [t|Xisampler2DMSArray|])
