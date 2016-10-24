{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Type where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)

type Embedded key = (Proxy key, String)
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show)
infixr 8 :<|>

embedded :: forall k (x :: k). String -> Embedded x
embedded val = (Proxy :: Proxy x, val) :: forall foo x. Embedded x

keys :: Embedded a :<|> Embedded b :<|> Embedded c -> [String]

instance Foo b => Foo ((:<|>) a b) where
  (a, b)
class Foo a where
  
