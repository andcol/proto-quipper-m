module Circuit.Gate where

data WireType = Bit | Qubit deriving (Show)

data Gate (sig :: [WireType]) where
    H :: Gate '[Qubit]
    X :: Gate '[Qubit]
    --more later