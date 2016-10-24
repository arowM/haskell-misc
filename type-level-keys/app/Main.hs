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
import Type ((:<|>)(..), Embedded, keys, kvs, embedded)
import Data (MyAPI)
import Text.Shakespeare.Text (textFile)

main :: IO ()
main = do
  putStrLn "Title: "
  a0 <- getLine
  TLIO.putStrLn . toLazyText . ($ ("" :: Text)) $
    $(withDict (keys (Proxy :: Proxy MyAPI))
      (textFile "app/test.html")
    )
    $ LHM.fromList $ kvs (Proxy :: Proxy MyAPI) $
      (embedded a0 :: Embedded "a") :<|>
      (embedded "var1" :: Embedded "b")
