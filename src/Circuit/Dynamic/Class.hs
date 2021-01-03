module Circuit.Dynamic.Class where

import Circuit.Gate
import Control.Monad.State.Lazy
import Data.Tuple

import GHC.TypeLits
import Data.Proxy

type Label = Int

type StaticLabel label = KnownNat label

l0 :: Proxy 0
l0 = Proxy
l1 :: Proxy 1
l1 = Proxy
l2 :: Proxy 2
l2 = Proxy
l3 :: Proxy 3
l3 = Proxy
l4 :: Proxy 4
l4 = Proxy
l5 :: Proxy 5
l5 = Proxy
l6 :: Proxy 6
l6 = Proxy
--etc...

reifyLabel :: forall l. StaticLabel l => Proxy l -> Int
reifyLabel = fromIntegral . natVal

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
