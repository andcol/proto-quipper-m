module Main where

import Types
import Interface
import DeepEmbedding
import Control.Monad.State.Lazy

import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Language.Core as Core
import Language.Lift as Lift
import Language.Box as Box


main :: IO ()
main = Box.test2

