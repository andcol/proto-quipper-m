module Main where

import Lib
import Language.Interface

--DEBUG
import Circuit.Class
import Data.Proxy


main :: IO ()
main = do
    let lc = reifyLabels (Proxy :: Proxy (1 : 1 : '[]))
    print lc
