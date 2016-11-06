{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TH where

import Language.Haskell.TH

import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.TypeLevelKVList (get, keys, keys', (:.)(..), Null(..), NamedVal, namedVal, NamedList)

test :: Q Exp
test = [|a <> b|]

args =
  (namedVal "str" :: NamedVal String "a") :.
  (namedVal "str2" :: NamedVal String "b") :. Null

{-|
  >>> $(withDict (Proxy :: Proxy (NamedVal String "a" :. NamedVal String "b" :. Null)) test) args
  "strstr2"
-}
withDict :: (NamedList a)
         => (Proxy a) -> Q Exp -> Q Exp
withDict p q = do
  let ls = keys p
  dict <- newName "dict"
  lamE [varP dict] $
    letE
      (map
        (\k ->
          (valD (varP (mkName k))
            (normalB
              (appE
                (appE (varE 'get)
                  (sigE (conE 'Proxy)
                    (appT (conT ''Proxy)
                      (litT (strTyLit k))
                    )
                  )
                )
                (varE dict)
              )
            )
            []
          )
        )
        ls
      )
      q
