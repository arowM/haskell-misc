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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

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
type NamedVal v key = (Proxy key, v)

{- | Type level list cons
 -}
data a :. b = a :. b
    deriving (Typeable, Eq, Show)
infixr 8 :.

{- | Type level @[]@
 -}
data Null = Null
    deriving (Typeable, Eq, Show)

type family Lookup pkey list where
  Lookup pk ((pk, v) :. b) = v
  Lookup pk ((px, v) :. b) = Lookup pk b
  Lookup pk Null = Null

{- | Main class for a list of values with type level key
 -}
class NamedList layout where
  type family NamedList' layout
  keys :: Proxy layout -> [String]

instance (NamedList b, KnownSymbol k) => NamedList (NamedVal v k :. b) where
  type NamedList' (NamedVal v k :. b) = NamedVal v k :. NamedList' b
  keys :: forall b0 k0 v0
       .  (NamedList b0, KnownSymbol k0)
       => Proxy (NamedVal v0 k0 :. b0) -> [String]
  keys _ = symbolVal (Proxy :: Proxy k0) : keys (Proxy :: Proxy b0)

instance NamedList Null where
  type NamedList' Null = Null
  keys :: forall v0
       .  Proxy Null -> [String]
  keys _ = []

{- | Chek if the key is included in type level list.
 -}
class HasKey list pkey value where
  get' :: pkey -> list -> value

instance (KnownSymbol k) =>
  HasKey (NamedVal v k :. b) (Proxy k) v where
  get' :: forall b0 k0 v0
      .  (KnownSymbol k0)
      => Proxy k0 -> NamedVal v0 k0 :. b0 -> v0
  get' _ ((_, a) :. _) = a

instance (NamedList b, KnownSymbol k, HasKey b (Proxy k) v) =>
  HasKey (a :. b) (Proxy k) v where
  get' _ (_ :. b) = get' (Proxy :: Proxy k) b

instance (KnownSymbol k, v ~ Null) =>
  HasKey Null (Proxy k) v where
  get' _ Null = Null

namedVal :: forall k v
         . (KnownSymbol k)
         => v -> NamedVal v k
namedVal a = (Proxy, a)

sampleList =
  (namedVal "str"   :: NamedVal String "foo") :.
  (namedVal 34      :: NamedVal Int "bar") :.
  (namedVal True    :: NamedVal Bool "baz") :. Null

get :: (HasKey list pkey (Lookup pkey list))
    => pkey -> list -> Lookup pkey list
get (pkey :: pkey) (list :: list) =
  get' pkey list :: Lookup pkey list
