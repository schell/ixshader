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
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.IxShader.Qualifiers where


import           Data.Promotion.Prelude         hiding (Const)
import           Data.Proxy
import           Data.Singletons.TypeLits
import           Prelude                        hiding (Read, return, (>>),
                                                 (>>=), log)

import           Graphics.IxShader.Function
import           Graphics.IxShader.IxShader
import           Graphics.IxShader.Types

newtype Uniform typ name = Uniform { unUniform :: typ }

instance KnownTypeSymbol t => KnownTypeSymbol (Uniform t n) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

instance Socketed t => Socketed (Uniform t n) where
  unSocket = unSocket . unUniform
  socket = Uniform . socket

newtype In typ name = In { unIn :: typ }

instance KnownTypeSymbol t => KnownTypeSymbol (In t n) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

instance Socketed t => Socketed (In t n) where
  unSocket = unSocket . unIn
  socket = In . socket

newtype Out typ name = Out { unOut :: typ }

instance KnownTypeSymbol t => KnownTypeSymbol (Out t n) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

instance Socketed t => Socketed (Out t n) where
  unSocket = unSocket . unOut
  socket = Out . socket

newtype Const typ = Const { unConst :: typ }

instance KnownTypeSymbol t => KnownTypeSymbol (Const t) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

instance Socketed t => Socketed (Const t) where
  unSocket = unSocket . unConst
  socket = Const . socket

newtype InOut typ = InOut { unInOut :: typ }

instance KnownTypeSymbol t => KnownTypeSymbol (InOut t) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

instance Socketed t => Socketed (InOut t) where
  unSocket = unSocket . unInOut
  socket = InOut . socket

-- Read and write rules
type family ReadFrom a where
  ReadFrom (Uniform t n) = t
  ReadFrom (In t n)      = t
  ReadFrom (Out t n)     = Error '(Out t n, "Cannot be read.")
  ReadFrom (InOut t)     = t
  ReadFrom (Const t)     = t
  ReadFrom t             = t

type family WriteTo a where
  WriteTo (Uniform t n) = Error '(Uniform t n, "Cannot be written.")
  WriteTo (In t n)      = Error '(In t n,      "Cannot be written.")
  WriteTo (Out t n)     = t
  WriteTo (InOut t)     = t
  WriteTo (Const t)     = Error '(Const t,     "Cannot be written.")
  WriteTo t             = t

class Cast a b where
  cast :: a -> b

instance (Socketed a, Socketed (ReadFrom a), b ~ ReadFrom a) => Cast a b where
  cast = socket . unSocket

type Readable a b = ( Socketed (ReadFrom a), Socketed a, Socketed b
                    , ReadFrom a ~ ReadFrom b
                    )

infixl 6 +
(+) :: Readable a b => a -> b -> ReadFrom a
(+) = callInfix "+"

infixl 6 -
(-) :: Readable a b => a -> b -> ReadFrom a
(-) = callInfix "-"

infixl 7 *
(*) :: Readable a b => a -> b -> ReadFrom a
(*) = callInfix "*"

negate :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
negate a = socket $ concat ["(-", unSocket a, ")"]

abs :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
abs = call "abs"

signum :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
signum = call "sign"

infixl 7 /
(/) :: Readable a b => a -> b -> ReadFrom a
(/) = callInfix "/"

exp :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
exp  = call "exp"

log :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
log  = call "log"

sqrt :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
sqrt = call "sqrt"

(**):: Readable a b => a -> b -> ReadFrom a
(**) = call2 "pow"

logBase :: Readable a b => a -> b -> ReadFrom a
logBase a b = callInfix "/" (log b) (log a)

sin :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
sin = call "sin"

cos :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
cos = call "cos"

tan :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
tan = call "tan"

asin :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
asin = call "asin"

acos :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
acos = call "acos"

atan :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
atan = call "atan"

sinh :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
sinh = call "sinh"

cosh :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
cosh = call "cosh"

tanh :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
tanh = call "tanh"

asinh :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
asinh = call "asinh"

acosh :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
acosh = call "acosh"

atanh :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
atanh = call "atanh"

infix 4 ==
(==) :: Readable a b => a -> b -> Xbool
(==) = callInfix "=="

infix 4 /=
(/=) :: Readable a b => a -> b -> Xbool
(/=) = callInfix "!="

infix 4 <
(<)  :: Readable a b => a -> b -> Xbool
(<) = callInfix "<"

infix 4 <=
(<=) :: Readable a b => a -> b -> Xbool
(<=) = callInfix "<="

infix 4 >
(>)  :: Readable a b => a -> b -> Xbool
(>) = callInfix ">"

infix 4 >=
(>=) :: Readable a b => a -> b -> Xbool
(>=) = callInfix ">="

max  :: Readable a b => a -> b -> ReadFrom a
max = call2 "max"

min  :: Readable a b => a -> b -> ReadFrom a
min = call2 "min"

normalize :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
normalize = call "normalize"

dot :: Readable a b => a -> b -> Xfloat
dot = call2 "dot"

reflect :: Readable a b => a -> b -> ReadFrom a
reflect = call2 "reflect"

inverse :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
inverse = call "inverse"

transpose :: (Socketed a, Socketed (ReadFrom a)) => a -> ReadFrom a
transpose = call "transpose"

inversesqrt :: Socketed a => a -> a
inversesqrt = call "inversesqrt"

clamp :: Socketed a => a -> a -> a -> a
clamp = call3 "clamp"

--------------------------------------------------------------------------------
-- Program-level in/out bindings
--------------------------------------------------------------------------------
class Binding a t where
  getVertexBinding  :: t
  getUniformBinding :: t

instance KnownSymbol b => Binding (Uniform a b) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Just $ symbolVal $ Proxy @b

instance KnownSymbol b => Binding (In a b) (Maybe String) where
  getVertexBinding = Just $ symbolVal $ Proxy @b
  getUniformBinding = Nothing

instance Binding (Out a b) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Nothing

instance Binding (Function a b c) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Nothing

instance Binding '[] [t] where
  getVertexBinding = []
  getUniformBinding = []

instance (Binding a t, Binding as [t]) => Binding (a ': as) [t] where
  getVertexBinding  = getVertexBinding  @a : getVertexBinding  @as
  getUniformBinding = getUniformBinding @a : getUniformBinding @as


-- | An easy way to get the term level value of a type of kind 'GLContext'.
class HasContext (a :: GLContext) where
  getCtx :: GLContext
instance HasContext 'OpenGLContext where
  getCtx = OpenGLContext
instance HasContext 'WebGLContext where
  getCtx = WebGLContext

-- | An easy way to get the term level value of a type of kind 'ShaderType'.
class HasShaderType (a :: ShaderType) where
  getShaderType :: ShaderType
instance HasShaderType 'VertexShader where
  getShaderType = VertexShader
instance HasShaderType 'FragmentShader where
  getShaderType = FragmentShader

uniform_
  :: forall t name ts ctx shadertype. (KnownSymbol name, Socketed t, KnownTypeSymbol t)
  => IxShader shadertype ctx ts (ts :++ '[Uniform t name]) (Uniform t name)
uniform_ = acc decls u u
  where
    u = socket $ symbolVal $ Proxy @name
    decls = unwords ["uniform", toDefinition u, ";"]

in_
  :: forall t name ts ctx shadertype.
     (HasContext ctx, HasShaderType shadertype, KnownSymbol name, Socketed t, KnownTypeSymbol t)
  => IxShader shadertype ctx ts (ts :++ '[In t name]) (In t name)
in_ = acc decls i i
  where
    i   = socket $ symbolVal $ Proxy @name
    dec = case (getCtx @ctx, getShaderType @shadertype) of
      (OpenGLContext, _)             -> "in"
      (WebGLContext, VertexShader)   -> "attribute"
      (WebGLContext, FragmentShader) -> "varying"
    decls = unwords [dec, toDefinition i, ";"]

out_
  :: forall t name ts ctx shadertype.
     (HasContext ctx, KnownSymbol name, Socketed t, KnownTypeSymbol t)
  => IxShader shadertype ctx ts (ts :++ '[Out t name]) (Out t name)
out_ = acc decls o o
  where
    o   = socket $ symbolVal $ Proxy @name
    dec = case getCtx @ctx of
      OpenGLContext -> "out"
      WebGLContext  -> "varying"
    decls = unwords [dec, toDefinition o, ";"]

gl_Position
  :: forall ts ctx.
  IxVertex ctx ts (ts :++ '[Out Xvec4 "gl_Position"]) (Out Xvec4 "gl_Position")
gl_Position = acc [] o o
  where o = socket "gl_Position"

type family GLFragName (a :: GLContext) where
  GLFragName 'OpenGLContext = "fragColor"
  GLFragName 'WebGLContext  = "gl_FragColor"

gl_FragColor
  :: forall ctx ts. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxFragment ctx ts (ts :++ '[Out Xvec4 (GLFragName ctx)]) (Out Xvec4 (GLFragName ctx))
gl_FragColor = acc decls o o
  where o = socket $ symbolVal $ Proxy @(GLFragName ctx)
        decls = case getCtx @ctx of
          OpenGLContext -> unwords ["out", toDefinition o, ";"]
          _             -> []

gl_FragCoord :: Xvec4
gl_FragCoord = Xvec4 "gl_FragCoord"
