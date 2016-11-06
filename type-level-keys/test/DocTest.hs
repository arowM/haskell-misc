module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options = doctest $ options <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [ "-XConstraintKinds"
    , "-XDataKinds"
    , "-XDeriveDataTypeable"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XEmptyDataDecls"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XInstanceSigs"
    , "-XMultiParamTypeClasses"
    , "-XNamedFieldPuns"
    , "-XNoImplicitPrelude"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTupleSections"
    , "-XTypeFamilies"
    , "-XTypeOperators"
    ]
