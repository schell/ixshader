{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Graphics.IxShader.Struct where

import           Data.Promotion.Prelude.List
import           Data.Promotion.Prelude.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Graphics.IxShader.IxShader
import           Graphics.IxShader.Types

newtype Struct (name :: Symbol) (fields :: [(Symbol, *)]) = Struct
    { unStruct :: String
    }

instance forall name fields. KnownSymbol name =>
         KnownTypeSymbol (Struct name fields) where
    typeSymbolVal _ = symbolVal (Proxy @name)

instance Socketed (Struct name fields) where
    unSocket = unStruct
    socket = Struct

class FieldDeclarations (a :: [(Symbol, *)]) where
    fieldDeclarations :: proxy a -> [String]

instance FieldDeclarations '[] where
    fieldDeclarations _ = []

instance forall name typ xs. ( KnownSymbol name
                             , KnownTypeSymbol typ
                             , FieldDeclarations xs
         ) =>
         FieldDeclarations ('( name, typ) ': xs) where
    fieldDeclarations _ = decl : fieldDeclarations (Proxy @xs)
      where
        decl =
            "\t" ++
            typeSymbolVal (Proxy @typ) ++ " " ++ symbolVal (Proxy @name) ++ ";"

type family FieldAt (f :: Symbol) (fs :: [(Symbol, *)]) where
    FieldAt f fs = 
        FromMaybe (TypeError ('Text "Struct field not found during lookup")) 
                  (Lookup f fs)

-- | Field accessor for structs
field :: forall a n fs field.
      (Socketed a, KnownSymbol n, KnownSymbol field, a ~ FieldAt field fs)
   => Struct n fs
   -> Proxy field
   -> a
field (Struct var) _ = socket $ var ++ "." ++ symbolVal (Proxy @field)

-- | Introduce a new struct
struct_ ::
       forall name fields ts ctx shadertype.
       ( FieldDeclarations fields
       , KnownTypeSymbol (Struct name fields)
       , Socketed (Struct name fields)
       )
    => IxShader shadertype ctx ts (ts :++ '[Struct name fields]) ()
struct_ = acc decls (Struct "" :: Struct name fields) ()
  where
    decls =
        unlines $
        ["struct " ++ typeSymbolVal (Proxy @(Struct name fields)) ++ "{"] ++
        fieldDeclarations (Proxy @fields) ++ ["};"]
