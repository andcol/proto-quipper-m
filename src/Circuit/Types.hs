module Circuit.Types where

import GHC.TypeLits

data WireType = Bit | Qubit
type Label = Nat

type LabelContext = [(Label,WireType)]

data Gate (sig :: [WireType]) where
    H :: Gate '[Qubit]
    X :: Gate '[Qubit]
    --more later