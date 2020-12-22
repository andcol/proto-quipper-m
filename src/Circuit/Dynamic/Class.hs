module Circuit.Dynamic.Class where

import Circuit.Gate
import Control.Monad.State.Lazy

type Label = Int 

type LabelContext = [(Label,WireType)]


class LabelledCircuit circ where
    fromGate :: Gate sig -> circ
    append :: circ -> [Label] -> circ -> (circ, [Label])

--Monadic helpers

appendM :: LabelledCircuit circ => [Label] -> circ -> State circ [Label]
appendM targets d = do
    c <- get
    let (c', k') = append c targets d 
    put c'
    return k'
