{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
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
module Graphics.IxShader.Function.ToParams where

import           Language.Haskell.TH
import           Graphics.IxShader.Socket
--------------------------------------------------------------------------------
-- Abstracting over function parameters.
--------------------------------------------------------------------------------
class ToParams a where
  toParams :: a -> [String]

instance ToParams () where
  toParams () = []

genToParams :: TypeQ -> DecsQ
genToParams t = [d|
  instance (Socketed $t, KnownTypeSymbol $t) => ToParams $t where
    toParams a = [toDefinition a]
  |]

instance (ToParams a, ToParams b) => ToParams (a, b) where
  toParams (a, b) = toParams a ++ toParams b

instance (ToParams a, ToParams b, ToParams c) => ToParams (a, b, c) where
  toParams (a, b, c) = toParams a ++ toParams b ++ toParams c

instance (ToParams a, ToParams b, ToParams c, ToParams d) => ToParams (a, b, c, d) where
  toParams (a, b, c, d) = toParams a ++ toParams b ++ toParams c ++ toParams d

toNames :: ToParams ps => ps -> [String]
toNames = map (drop 1 . dropWhile (/= ' ')) . toParams
