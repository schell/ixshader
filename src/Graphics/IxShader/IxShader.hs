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
module Graphics.IxShader.IxShader
  ( GLContext(..)
  , ShaderType(..)
  , IxShader
  , IxVertex
  , IxFragment
  , unDecl
  , unN
  , (>>=)
  , (>>)
  , return
  , fail
  , void
  , acc
  , nxt
  , nxt_
  , sub
  , sub_
  , pop
  , putSrcLn
  , onlySrc
  , toSrc
  , ixShaderSrc
  ) where

import           Control.Arrow                  ((&&&))
import           Control.Monad.Indexed
import           Data.Promotion.Prelude.List    ((:++))
import Data.List (isSuffixOf)
import           Language.GLSL                  (TranslationUnit (..), parse)
import           Prelude                        hiding (Read, return, (>>),
                                                 (>>=), fail)
import           Text.PrettyPrint.HughesPJClass hiding (int)


data ShaderType = VertexShader
                | FragmentShader

-- | Some glsl evaluation contexts. This is used to choose alternate syntax in
-- cases where shader code differs between contexts, for example the @in@ keyword
-- is not available on glsl bound for a webgl context, and should be replaced
-- with @attribute@.
data GLContext = OpenGLContext
               | WebGLContext

data IxShader (shadertype :: ShaderType) (ctx :: GLContext) i j n where
  ShNxt :: [String] -> n -> IxShader shadertype ctx i j n
  ShAcc :: [String] -> t -> n -> IxShader shadertype ctx i (i :++ '[t]) n
  ShPop :: n -> IxShader shadertype ctx (t ': j) j n

type IxVertex = IxShader 'VertexShader
type IxFragment = IxShader 'FragmentShader

unN :: IxShader shadertype ctx i j n -> n
unN = \case
  ShNxt _ n   -> n
  ShAcc _ _ n -> n
  ShPop n     -> n

unDecl :: IxShader shadertype ctx i j n -> [String]
unDecl = \case
  ShNxt d _   -> d
  ShAcc d _ _ -> d
  ShPop _     -> []

instance IxFunctor (IxShader shadertype ctx) where
  imap f sh = ShNxt (unDecl sh) $ f (unN sh)

instance IxPointed (IxShader shadertype ctx) where
  ireturn = ShNxt []

instance IxApplicative (IxShader shadertype ctx) where
  iap mf mx = ShNxt (unDecl mf ++ unDecl mx) $ unN mf $ unN mx

instance IxMonad (IxShader shadertype ctx) where
  ibind amb ma =
    let (dsa, a) = unDecl &&& unN $ ma
        (dsb, b) = unDecl &&& unN $ amb a
    in ShNxt (dsa ++ dsb) b

fail :: forall i j a shadertype ctx. String -> IxShader shadertype ctx i j a
fail = error

(>>=) :: forall i j k a b shadertype ctx. IxShader shadertype ctx i j a -> (a -> IxShader shadertype ctx j k b) -> IxShader shadertype ctx i k b
a >>= b = a >>>= b

return :: forall a i shadertype ctx. a -> IxShader shadertype ctx i i a
return = ireturn

(>>) :: forall i j a k b shadertype ctx. IxShader shadertype ctx i j a -> IxShader shadertype ctx j k b -> IxShader shadertype ctx i k b
a >> b = a >>>= const b

void :: IxShader shadertype ctx i k a -> IxShader shadertype ctx i k ()
void ma = ma >> return ()

-- | Does three things - appends a type to the IxMonad @j@, encodes one or more
-- lines of shader code and returns something. This is the main entry point for
-- any shader building code, and also an easy escape hatch.
acc
  :: forall typ a i shadertype ctx. String
  -> typ
  -> a
  -> IxShader shadertype ctx i (i :++ '[typ]) a
acc dec = ShAcc (lines dec)

nxt
  :: forall i a ctx shadertype.
     String
  -> a
  -> IxShader shadertype ctx i i a
nxt dec = ShNxt (lines dec)

nxt_ :: forall i shadertype ctx. String -> IxShader shadertype ctx i i ()
nxt_ dec = nxt dec ()

sub
  :: forall i j a ctx shadertype.
     String
  -> String
  -> IxShader shadertype ctx i j a
  -> IxShader shadertype ctx i j a
sub open close sh = do
  nxt open ()
  a <- sh
  nxt close ()
  return a

sub_
  :: forall i j a shadertype ctx.
     String
  -> String
  -> IxShader shadertype ctx i j a
  -> IxShader shadertype ctx i j ()
sub_ open close sh = sub open close sh >> return ()

pop
  :: IxShader shadertype ctx (t ': j) j ()
pop = ShPop ()

--------------------------------------------------------------------------------
-- From IxShader to GLSL
--------------------------------------------------------------------------------
fromIxShader :: IxShader shadertype ctx '[] j a -> Either String TranslationUnit
fromIxShader = showLeft . parse . unlines . unDecl
  where showLeft = \case
          Left err  -> Left $ show err
          Right ast -> Right ast

toSrc :: Pretty a => a -> String
toSrc = show . pPrint

onlySrc
  :: forall ctx shadertype i j a.
     IxShader shadertype ctx i j a
  -> String
onlySrc = unlines . snd . foldl indent (0, []) . unDecl
  where ndnt = "  "
        incIndent n ln
          | "{" `isSuffixOf` ln = n + 1
          | "}" `isSuffixOf` ln = n - 1
          | otherwise           = n
        indent (n, decls) ln = (incIndent n ln, decls ++ [concat (replicate n ndnt) ++ ln])

ixShaderSrc
  :: forall ctx shadertype j a.
     IxShader shadertype ctx '[] j a
  -> Either String String
ixShaderSrc = fmap toSrc . fromIxShader

putSrcLn :: forall shadertype ctx j a. IxShader shadertype ctx '[] j a -> IO ()
putSrcLn = either putStrLn (putStrLn . toSrc) . fromIxShader
