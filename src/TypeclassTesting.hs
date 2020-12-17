module TypeclassTesting where

import Data.Proxy
import GHC.TypeLits
import Data.Type.Bool

class KnownNatList (l :: [Nat])
instance KnownNatList '[]
instance KnownNatList r => KnownNatList (e ': r)

type family Max (n1 :: Nat) (n2 :: Nat) :: Nat where
    Max n n = n
    Max n1 n2 = If (n1 <=? n2) n2 n1

type family Maximum (ns :: [Nat]) :: Nat where
    Maximum '[n] = n
    Maximum (n ': rn) = Max n (Maximum rn)

type family FindFresh (ns :: [Nat]) :: Nat where
    FindFresh ns = (Maximum ns) + 1


maxInWitness :: forall ns proxy. KnownNatList ns => proxy ns -> Proxy (Maximum ns)
maxInWitness _ = undefined

freshInWitness :: forall ns proxy. KnownNatList ns => proxy ns -> Proxy (FindFresh ns)
freshInWitness _ = undefined

--Maybe we do not need polymorphism on proxy, not confortable with those undefined