module Circuit.Gate where

data WireType = Bit | Qubit deriving (Show, Eq)

subscripts = "₀₁₂₃₄₅₆₇₈₉"
showSubscript :: Int -> String
showSubscript n | n >= 0 && n < 10 = [subscripts !! n]
showSubscript n = (showSubscript $ n `div` 10) ++ [subscripts !! (n `mod` 10)]

type family Controlled (sig :: [WireType]) :: [WireType] where
    Controlled sig = Qubit : sig

data Gate (sig :: [WireType]) where
    Meas :: Gate '[Qubit]
    H :: Gate '[Qubit]
    X :: Gate '[Qubit]
    R :: Int -> Gate '[Qubit]
    C :: Gate sig -> Gate (Controlled sig)
    --more later

instance Show (Gate sig) where
    show Meas = "measure"
    show H = "H"
    show X = "X"
    show (R m) = "R" ++ (showSubscript m)
    show (C X) = "CNOT"
    show (C g) = "C" ++ (show g)

instance Eq (Gate sig) where
    Meas == Meas = True
    H == H = True
    X == X = True
    (R m) == (R k) = m == k
    (C g) == (C h) = g == h
    g == h = False
