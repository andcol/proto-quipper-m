module Main where

import Types
import Interface
import DeepEmbedding
import Control.Monad.State.Lazy

import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Language.Interface
import Language.Core as Core


main :: IO ()
main = Core.test2

