{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.HashMap.Lazy as LHM
import Data.Text (Text)
import Data.Text.Internal.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TLIO
import TH
import Data.Proxy (Proxy(..))
import Data (MyAPI)
import Data.TypeLevelKVList ((:.)(..), NamedVal, keys, namedVal)
import Text.Shakespeare.Text (textFile)

main :: IO ()
main = do
  putStrLn "Title: "
  a0 <- getLine
  let
    a = a0
    b = "foo" :: String
    f v = "<span>v</span>" :: String
  TLIO.putStrLn . toLazyText . ($ ("" :: Text)) $
    $(textFile "app/test.html")

    -- $(withDict (keys (Proxy :: Proxy MyAPI))
    --   (textFile "app/test.html")
    -- )
    -- $ LHM.fromList $ kvs (Proxy :: Proxy MyAPI) $
    --   (namedVal a0 :: NamedVal "a") :.
    --   (namedVal "var1" :: NamedVal "b")
