{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.HashMap.Lazy as LHM
import Data.Text (Text)
import TH

main :: IO ()
main = do
  a0 <- readLn
  let
    dict = LHM.fromList . zipWith (,) ["a"] $
      [ a0
      , "bar"
      ] :: LHM.HashMap String Text
  print $ let ls = ["a"] in $(withDict test) dict
