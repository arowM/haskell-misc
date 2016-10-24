{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Type where
{- | The goal is to declare the type level keyed values like following.
    kvs :: Embedded "foo" :<|> Embedded "bar" :<|> Embedded "baz"
    kvs =
      (Embedded "val0" :: Embedded "foo") :<|>
      (Embedded "val1" :: Embedded "bar") :<|>
      (Embedded "val2" :: Embedded "baz")

    keys kvs == ["foo", "bar", "baz"]
    get (Proxy :: Proxy "foo") kvs == "val0"
-}

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

type Embedded key = (Proxy key, String)
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show)
infixr 8 :<|>

class HasServer layout where
  type family Server layout
  kvs :: Proxy layout -> Server layout -> [(String, String)]

instance (HasServer b, KnownSymbol x) => HasServer (Embedded x :<|> b) where
  type Server (Embedded x :<|> b) = Embedded x :<|> Server b
  kvs :: forall a b x
      .  (HasServer b, Embedded x ~ a, KnownSymbol x)
      => Proxy (Embedded x :<|> b) -> Embedded x :<|> Server b -> [(String, String)]
  kvs _ ((_, a) :<|> b) = (symbolVal (Proxy :: Proxy x), a) : kvs (Proxy :: Proxy b) b

instance (KnownSymbol x) => HasServer (Embedded x) where
  type Server (Embedded x) = Embedded x
  kvs :: forall a x
      .  (Embedded x ~ a, KnownSymbol x)
      => Proxy (Embedded x) -> Embedded x -> [(String, String)]
  kvs _ (_, a) = [(symbolVal (Proxy :: Proxy x), a)]

embedded :: forall x
         .  (KnownSymbol x)
         => String -> Embedded x
embedded val = (Proxy :: Proxy x, val)

type MyAPI = Embedded "foo" :<|> Embedded "bar" :<|> Embedded "baz"

myKvs = kvs (Proxy :: Proxy MyAPI) $
  embedded "var0" :<|>
  embedded "var1" :<|>
  embedded "var2"
