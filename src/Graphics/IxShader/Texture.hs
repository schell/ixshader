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
module Graphics.IxShader.Texture where

import           Prelude                     hiding (Read, return, (>>), (>>=))


import           Graphics.IxShader.Socket     as G
import           Graphics.IxShader.Qualifiers     as G
import           Graphics.IxShader.Types      as G

type family TextureSize a where
  TextureSize Xsampler1D = Xint
  TextureSize Xisampler1D = Xint
  TextureSize Xusampler1D = Xint
  TextureSize Xsampler2D  = Xivec2
  TextureSize Xisampler2D = Xivec2
  TextureSize Xusampler2D = Xivec2
  TextureSize Xsampler3D  = Xivec3
  TextureSize Xisampler3D = Xivec3
  TextureSize Xusampler3D = Xivec3
  TextureSize XsamplerCube  = Xivec2
  TextureSize XisamplerCube = Xivec2
  TextureSize XusamplerCube = Xivec2
  TextureSize Xsampler1DShadow = Xint
  TextureSize Xsampler2DShadow = Xivec2
  TextureSize XsamplerCubeShadow = Xivec2
  TextureSize Xsampler2DRect = Xivec2
  TextureSize Xsampler2DRectShadow = Xivec2
  TextureSize Xsampler1DArray  = Xivec2
  TextureSize Xisampler1DArray = Xivec2
  TextureSize Xusampler1DArray = Xivec2
  TextureSize Xsampler2DArray  = Xivec3
  TextureSize Xisampler2DArray = Xivec3
  TextureSize Xusampler2DArray = Xivec3
  TextureSize Xsampler1DArrayShadow = Xivec2
  TextureSize Xsampler2DArrayShadow = Xivec3
  TextureSize XsamplerBuffer  = Xint
  TextureSize XisamplerBuffer = Xint
  TextureSize XusamplerBuffer = Xint
  TextureSize Xsampler2DMS  = Xivec2
  TextureSize Xisampler2DMS = Xivec2
  TextureSize Xusampler2DMS = Xivec2
  TextureSize Xsampler2DMSArray  = Xivec2
  TextureSize Xisampler2DMSArray = Xivec2
  TextureSize Xusampler2DMSArray = Xivec2

textureSize :: (Socketed a, Socketed (TextureSize (ReadFrom a))) => a -> TextureSize (ReadFrom a)
textureSize = call "textureSize"

textureSizeLOD :: (Socketed a, Socketed int, Socketed (TextureSize (ReadFrom a)), ReadFrom int ~ Xint) => a -> int -> TextureSize (ReadFrom a)
textureSizeLOD = call2 "textureSize"

type family Texture a where
  Texture Xsampler1D = Xvec4
  Texture Xisampler1D = Xivec4
  Texture Xusampler1D = Xuvec4
  Texture Xsampler2D = Xvec4
  Texture Xisampler2D = Xivec4
  Texture Xusampler2D = Xuvec4
  Texture Xsampler3D = Xvec4
  Texture Xisampler3D = Xivec4
  Texture Xusampler3D = Xuvec4
  Texture XsamplerCube = Xvec4
  Texture XisamplerCube = Xivec4
  Texture XusamplerCube = Xuvec4
  Texture Xsampler1DShadow = Xfloat
  Texture Xsampler2DShadow = Xfloat
  Texture XsamplerCubeShadow = Xfloat
  Texture Xsampler1DArray = Xvec4
  Texture Xisampler1DArray = Xivec4
  Texture Xusampler1DArray = Xuvec4
  Texture Xsampler2DArray = Xvec4
  Texture Xisampler2DArray = Xivec4
  Texture Xusampler2DArray = Xuvec4
  Texture Xsampler1DArrayShadow = Xfloat
  Texture Xsampler2DArrayShadow = Xfloat
  Texture Xsampler2DRect = Xvec4
  Texture Xisampler2DRect = Xivec4
  Texture Xusampler2DRect = Xuvec4
  Texture Xsampler2DRectShadow = Xfloat

type family TextureLookup a where
  TextureLookup Xsampler1D  = Xfloat
  TextureLookup Xisampler1D = Xfloat
  TextureLookup Xusampler1D = Xfloat
  TextureLookup Xsampler2D = Xvec2
  TextureLookup Xisampler2D = Xvec2
  TextureLookup Xusampler2D = Xvec2
  TextureLookup Xsampler3D = Xvec3
  TextureLookup Xisampler3D = Xvec3
  TextureLookup Xusampler3D = Xvec3
  TextureLookup XsamplerCube = Xvec3
  TextureLookup XisamplerCube = Xvec3
  TextureLookup XusamplerCube = Xvec3
  TextureLookup Xsampler1DShadow = Xvec3
  TextureLookup Xsampler2DShadow = Xvec3
  TextureLookup XsamplerCubeShadow = Xvec4
  TextureLookup Xsampler1DArray = Xvec2
  TextureLookup Xisampler1DArray = Xvec2
  TextureLookup Xusampler1DArray = Xvec2
  TextureLookup Xsampler2DArray = Xvec3
  TextureLookup Xisampler2DArray = Xvec3
  TextureLookup Xusampler2DArray = Xvec3
  TextureLookup Xsampler1DArrayShadow = Xvec3
  TextureLookup Xsampler2DArrayShadow = Xvec4
  TextureLookup Xsampler2DRect = Xvec2
  TextureLookup Xisampler2DRect = Xvec2
  TextureLookup Xusampler2DRect = Xvec2
  TextureLookup Xsampler2DRectShadow = Xvec3

class AcceptsTextureLookupBias a where
instance AcceptsTextureLookupBias Xsampler1D
instance AcceptsTextureLookupBias Xisampler1D
instance AcceptsTextureLookupBias Xusampler1D
instance AcceptsTextureLookupBias Xsampler2D
instance AcceptsTextureLookupBias Xisampler2D
instance AcceptsTextureLookupBias Xusampler2D
instance AcceptsTextureLookupBias Xsampler3D
instance AcceptsTextureLookupBias Xisampler3D
instance AcceptsTextureLookupBias Xusampler3D
instance AcceptsTextureLookupBias XsamplerCube
instance AcceptsTextureLookupBias XisamplerCube
instance AcceptsTextureLookupBias XusamplerCube
instance AcceptsTextureLookupBias Xsampler1DShadow
instance AcceptsTextureLookupBias Xsampler2DShadow
instance AcceptsTextureLookupBias XsamplerCubeShadow
instance AcceptsTextureLookupBias Xsampler1DArray
instance AcceptsTextureLookupBias Xisampler1DArray
instance AcceptsTextureLookupBias Xusampler1DArray
instance AcceptsTextureLookupBias Xsampler2DArray
instance AcceptsTextureLookupBias Xisampler2DArray
instance AcceptsTextureLookupBias Xusampler2DArray
instance AcceptsTextureLookupBias Xsampler1DArrayShadow

textureBias
  :: (AcceptsTextureLookupBias a, Socketed a, Socketed (TextureLookup a), Socketed (Texture (ReadFrom a)), Socketed float, ReadFrom float ~ Xfloat)
  => a
  -> TextureLookup a
  -> float
  -> Texture (ReadFrom a)
textureBias = call3 "texture"

texture
  :: (Socketed a, Socketed (TextureLookup a), Socketed (Texture (ReadFrom a)))
  => a
  -> TextureLookup a
  -> Texture (ReadFrom a)
texture = call2 "texture"
