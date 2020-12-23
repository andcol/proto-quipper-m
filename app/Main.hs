module Main where

import Lib
import Language.Interface
import Circuit.Dynamic.Naive
import Circuit.Gate
import Circuit.Dynamic.Class


main :: IO ()
main = do
    let h = append (hadamard 10) [10] (fromGate (R 2))
    print h

