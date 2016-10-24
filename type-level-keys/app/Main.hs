{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.HashMap.Lazy as LHM
-- import Data.Text (Text)
import TH
import Data.Proxy (Proxy(..))
import Type ((:<|>)(..), keys, kvs, embedded)
import Data (MyAPI)

main :: IO ()
main = do
  a0 <- getLine
  putStrLn $
    $(withDict (keys (Proxy :: Proxy MyAPI)) test)
    $ LHM.fromList $ kvs (Proxy :: Proxy MyAPI) $
      embedded a0 :<|>
      embedded "var1"
