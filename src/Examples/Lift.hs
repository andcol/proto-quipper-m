module Examples.Lift where

import Types
import Interface hiding (lift, force)
import DeepEmbedding
import Circuit.Gate
import Circuit.Dynamic.Class
import Circuit.Dynamic.Naive

import Control.Monad.State.Lazy hiding (lift)

import Language.Lift

instance Show (LVal Deep (a ⊸ b)) where
    show _ = "[function]"

test1 :: IO () --simple "force (lift λx.x)" scenario
test1 = do
    let expr = force @Deep $ lift $ λ (\x -> x)
    let res = eval expr eEmpty
    let out = runState res (identity [(0,Qubit)]) --initial state is irrelevant
    print out

--test2 :: IO () --same scenario but with inner evaluation
--test2 = do
--    let expr = force @Deep $ lift $ apply (circuit (VLabel 0) (fromGate H) (VLabel 1)) (label l0) --again, should not work, but for now labels are typed without Q
--    let res = eval expr eEmpty
--    let vs = runState res (identity [(0,Qubit)])
--    print vs