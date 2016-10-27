{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data where

import Type ((:.), NamedVal)

type MyAPI = NamedVal "a" :. NamedVal "b"
