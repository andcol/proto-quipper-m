module Circuit.Effect where

import Data.Proxy
import Circuit.Types
import Circuit.Class
import Control.Monad.State.Lazy


appendM :: (LabelledCircuit circ, LabelList k, LabelList k') => Proxy k -> circ q3 q4 -> State (circ q1 q6) (Proxy k')
appendM targets d = do
    c <- get --TODO the problem is that in C:q1->q2 q2 is ambiguous and cannot be resolved
    let (c', k') = append c targets d
    put c'
    return k'
