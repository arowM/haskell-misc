{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.TypeLevelKVList (get, keys, (:.)(..), Null(..), NamedVal, namedVal)

test :: Q Exp
test = [|a <> b|]

args =
  (namedVal "str" :: NamedVal String "a") :.
  (namedVal "str2" :: NamedVal String "b") :. Null

withDict :: [String] -> Q Exp -> Q Exp
withDict ls q = do
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
