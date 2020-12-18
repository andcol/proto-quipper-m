module Circuit.Gate where

import Circuit.LabelContext

data Gate (sig :: [WireType]) where
    H :: Gate '[Qubit]
    X :: Gate '[Qubit]
    --more later