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
module Graphics.IxShader.Function
  ( module Graphics.IxShader.Function
  , (:++)
  ) where

import           Data.List                          (intercalate)
import           Data.Promotion.Prelude
import           Data.Singletons.TypeLits
import           Prelude                            hiding (Read, return, (>>),
                                                     (>>=))

import           Graphics.IxShader.Function.ToParams
import           Graphics.IxShader.IxShader
import           Graphics.IxShader.Socket
import           Graphics.IxShader.Types             (Xvoid)

--------------------------------------------------------------------------------
-- Defining and calling functions
-------------------------------------------------------------------------------
funcReturnType :: forall t ctx shadertype i. (KnownTypeSymbol t) => IxShader shadertype ctx i i ()
funcReturnType = nxt_ $ typeSymbolVal $ Proxy @t

funcName :: forall name ctx shadertype i. (KnownSymbol name) => IxShader shadertype ctx i i ()
funcName = nxt_ $ symbolVal $ Proxy @name

funcParams :: ToParams ps => ps -> IxShader shadertype ctx i i ()
funcParams ps = nxt_ $ "(" ++ intercalate ", " (toParams ps) ++ ")"

returnValue
  :: (Socketed a, KnownTypeSymbol a)
  => a -> IxShader shadertype ctx i i a
returnValue a = nxt (unwords ["return", unSocket a, ";"]) a

funcCall
  :: forall name t ps. (KnownSymbol name, Socketed t, ToParams ps)
  => ps
  -> t
funcCall ps = socket $ unwords [ symbolVal $ Proxy @name
                               , "("
                               , intercalate ", " $ toNames ps
                               , ")"
                               ]

data Function rtype fname ps = Function

type IxFunction shadertype ctx i rtype fname ps =
  IxShader shadertype ctx i (i :++ '[Function rtype fname ps]) (ps -> rtype)

func
  :: forall fname rtype ps ctx shadertype i.
     (ToParams ps, KnownTypeSymbol rtype, Socketed rtype, KnownSymbol fname)
  => ps
  -> (ps -> IxShader shadertype ctx i i rtype)
  -> IxShader shadertype ctx i (i :++ '[Function rtype fname ps]) (ps -> rtype)
func ps f = do
  nxt_ ""
  funcReturnType @rtype
  funcName @fname
  funcParams ps
  sub_ "{" "}" $ f ps
  acc "" (Function @rtype @fname @ps) ()
  nxt_ ""
  return $ funcCall @fname

use :: Socketed a => a -> IxShader shadertype ctx i i ()
use a = nxt_ (unSocket a ++ ";")

type Main = Function Xvoid "main" ()
