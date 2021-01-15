-----------------------------------------------
-- This module re-exports all and only the   --
-- LNLHaskell modules needed for the deep    --
-- embedding of Proto-Quipper-M.             --
-----------------------------------------------

module LNLHask (
    module Types,
    module Classes,
    module Interface,
    module DeepEmbedding
) where

import Types
import Classes
import Interface hiding (Bang, lift, force)
import DeepEmbedding