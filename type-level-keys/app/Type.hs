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
import GHC.TypeLits (KnownSymbol, symbolVal)

type Embedded key = (Proxy key, String)
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show)
infixr 8 :<|>

class HasServer layout where
  type family Server layout
  kvs :: Proxy layout -> Server layout -> [(String, String)]
  keys :: Proxy layout -> [String]

instance (HasServer b, KnownSymbol x) => HasServer (Embedded x :<|> b) where
  type Server (Embedded x :<|> b) = Embedded x :<|> Server b
  kvs :: forall b0 x0
      .  (HasServer b0, KnownSymbol x0)
      => Proxy (Embedded x :<|> b0) -> Embedded x0 :<|> Server b0 -> [(String, String)]
  kvs _ ((_, a) :<|> b) = (symbolVal (Proxy :: Proxy x0), a) : kvs (Proxy :: Proxy b0) b


  keys :: forall b0 x0
       .  (HasServer b0, KnownSymbol x0)
       => Proxy (Embedded x0 :<|> b0) -> [String]
  keys _ = symbolVal (Proxy :: Proxy x0) : keys (Proxy :: Proxy b0)

instance (KnownSymbol x) => HasServer (Embedded x) where
  type Server (Embedded x) = Embedded x
  kvs :: forall x0
      .  (KnownSymbol x0)
      => Proxy (Embedded x0) -> Embedded x0 -> [(String, String)]
  kvs _ (_, a) = [(symbolVal (Proxy :: Proxy x0), a)]

  keys :: forall x0
       .  (KnownSymbol x0)
       => Proxy (Embedded x0) -> [String]
  keys _ = [symbolVal (Proxy :: Proxy x0)]

embedded :: forall x. String -> Embedded x
embedded val = (Proxy :: Proxy x, val)

type MyAPI = Embedded "foo" :<|> Embedded "bar" :<|> Embedded "baz"
myKvs :: [(String, String)]
myKvs = kvs (Proxy :: Proxy MyAPI) $
  (embedded "var0" :: Embedded "foo") :<|>
  (embedded "var1" :: Embedded "bar") :<|>
  (embedded "var2" :: Embedded "baz")
