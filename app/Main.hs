module Main where

import Lib
import Language.Interface

--DEBUG
import Circuit.Class
import Circuit.Class
import TypeclassTesting
import Data.Proxy
import GHC.TypeLits

main :: IO ()
main = do
    let n = natVal . freshInWitness $ (Proxy :: Proxy [1,6,3,4])
    print n
