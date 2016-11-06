{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data where

import Data.TypeLevelKVList ((:.), NamedVal)

type MyAPI = NamedVal String "a" :. NamedVal String "b"
