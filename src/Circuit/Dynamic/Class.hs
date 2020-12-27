module Circuit.Dynamic.Class where

import Circuit.Gate
import Control.Monad.State.Lazy
import Data.Tuple

type Label = Int 

type LabelContext = [(Label,WireType)]

labelsOf :: LabelContext -> [Label]
labelsOf = map fst
semanticsOf :: LabelContext -> [WireType]
semanticsOf = map snd

class LabelledCircuit circ where
    identity :: LabelContext -> circ
    fromGate :: Gate sig -> circ
    append :: circ -> [Label] -> circ -> (circ, [Label])

--Monadic helpers

appendM :: LabelledCircuit circ => [Label] -> circ -> State circ [Label]
appendM targets d = do
    c <- get
    let (c', k') = append c targets d 
    put c'
    return k'
