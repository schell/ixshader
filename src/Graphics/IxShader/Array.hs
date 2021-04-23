{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.IxShader.Array where

import           Data.Proxy
import           GHC.TypeLits
import           Graphics.IxShader.Socket
import           Graphics.IxShader.Qualifiers
import           Graphics.IxShader.Types

newtype Array (n :: Nat) a = Array { unArray :: String }

instance Socketed (Array n b) where
    unSocket = unArray
    socket = Array

instance forall n a. (KnownNat n, KnownTypeSymbol a) =>
         KnownTypeSymbol (Array n a) where
    typeSymbolVal _ =
        typeSymbolVal (Proxy @a) ++ "[" ++ show (natVal $ Proxy @n) ++ "]"

ix :: (Socketed a, Socketed b, ReadFrom a ~ Array n b) => a -> Xint -> b
ix a i = socket $ unSocket a ++ "[" ++ unSocket i ++ "]"
