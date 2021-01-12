module Main where

import Types
import Interface
import DeepEmbedding
import Control.Monad.State.Lazy

import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Examples.Core as Core
import Examples.Lift as Lift
import Examples.Box as Box


main :: IO ()
main = Box.test2

