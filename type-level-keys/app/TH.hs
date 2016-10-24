{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

-- import Language.Haskell.TH.Syntax (liftData)
import Data.Monoid ((<>))
import Data.HashMap.Lazy ((!))

test :: Q Exp
test = [|a <> b|]

multi :: Name -> Q Exp
multi n = [| ($(varE n) *) |]

withDict :: Q Exp -> Q Exp
withDict q = do
  let ls = $(varE $ mkName "ls")
  dict <- newName "dict"
  lamE [varP dict] $
    letE
      (map
         (\k ->
           valD
            (varP (mkName k))
            (normalB (appE (appE (varE '(!)) (varE dict)) (litE (stringL k))))
            []
         )
         ls
      )
      q
