{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data where

import Type ((:<|>), Embedded)

type MyAPI = Embedded "a" :<|> Embedded "b"
