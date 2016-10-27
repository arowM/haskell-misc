{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Type where
{- | The goal is to declare the type level keyed values like following.
    kvs :: NamedVal "foo" :. NamedVal "bar" :. NamedVal "baz"
    kvs =
      (NamedVal "val0" :: NamedVal "foo") :.
      (NamedVal "val1" :: NamedVal "bar") :.
      (NamedVal "val2" :: NamedVal "baz")

    keys kvs == ["foo", "bar", "baz"]
    get (Proxy :: Proxy "foo") kvs == "val0"
-}

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, symbolVal)

{- | A value with type level index.
 -}
type NamedVal key = (Proxy key, String)

{- | Type level list cons
 -}
data a :. b = a :. b
    deriving (Typeable, Eq, Show)
infixr 8 :.

class NamedList layout where
  type family NamedList' layout
  kvs :: Proxy layout -> NamedList' layout -> [(String, String)]
  keys :: Proxy layout -> [String]

instance (NamedList b, KnownSymbol x) => NamedList (NamedVal x :. b) where
  type NamedList' (NamedVal x :. b) = NamedVal x :. NamedList' b
  kvs :: forall b0 x0
      .  (NamedList b0, KnownSymbol x0)
      => Proxy (NamedVal x :. b0) -> NamedVal x0 :. NamedList' b0 -> [(String, String)]
  kvs _ ((_, a) :. b) = (symbolVal (Proxy :: Proxy x0), a) : kvs (Proxy :: Proxy b0) b


  keys :: forall b0 x0
       .  (NamedList b0, KnownSymbol x0)
       => Proxy (NamedVal x0 :. b0) -> [String]
  keys _ = symbolVal (Proxy :: Proxy x0) : keys (Proxy :: Proxy b0)

instance (KnownSymbol x) => NamedList (NamedVal x) where
  type NamedList' (NamedVal x) = NamedVal x
  kvs :: forall x0
      .  (KnownSymbol x0)
      => Proxy (NamedVal x0) -> NamedVal x0 -> [(String, String)]
  kvs _ (_, a) = [(symbolVal (Proxy :: Proxy x0), a)]

  keys :: forall x0
       .  (KnownSymbol x0)
       => Proxy (NamedVal x0) -> [String]
  keys _ = [symbolVal (Proxy :: Proxy x0)]

namedVal :: forall x. String -> NamedVal x
namedVal val = (Proxy :: Proxy x, val)

type MyAPI = NamedVal "foo" :. NamedVal "bar" :. NamedVal "baz"
myKvs :: [(String, String)]
myKvs = kvs (Proxy :: Proxy MyAPI) $
  (namedVal "var0" :: NamedVal "foo") :.
  (namedVal "var1" :: NamedVal "bar") :.
  (namedVal "var2" :: NamedVal "baz")
