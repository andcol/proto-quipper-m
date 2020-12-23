module Main where

import Types
import Interface
import DeepEmbedding
import Control.Monad.State.Lazy

import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Language.Interface
import Language.Core


main :: IO ()
main = do
    let expr = apply @Deep (circuit (label 0) (fromGate H) (label 1)) (label 0)
    let res = eval expr eEmpty
    let (_,s) = runState res (Identity [(0,Qubit)])
    print s

