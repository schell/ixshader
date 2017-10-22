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
module Graphics.IxShader.Types.Xbool where


import           Prelude                 hiding (Ord (..), Read, return, (>>),
                                          (>>=), Eq (..))

import           Graphics.IxShader.Socket
import           Graphics.IxShader.Function.ToParams
import           Graphics.IxShader.IxShader


newtype Xbool = Xbool { unXbool :: String }
$(genKnownTypeSymbol [t|Xbool|] [e|"bool"|])
$(genSocketed        [t|Xbool|] [e|unXbool|] [e|Xbool|])
$(genToParams        [t|Xbool|])

ifThenElse :: Xbool -> IxShader shadertype ctx i i () -> IxShader shadertype ctx i i () -> IxShader shadertype ctx i i ()
ifThenElse x a b = do
  nxt_ $ "if (" ++ unSocket x ++ ")"
  sub_ "{" "}" a
  sub_ "else {" "}" b

when :: Xbool -> IxShader shadertype ctx i i () -> IxShader shadertype ctx i i ()
when x a = do
  nxt_ $ "if (" ++ unSocket x ++ ")"
  sub_ "{" "}" a

unless :: Xbool -> IxShader shadertype ctx i i () -> IxShader shadertype ctx i i ()
unless x a = do
  nxt_ $ "if (! " ++ unSocket x ++ ")"
  sub_ "{" "}" a
