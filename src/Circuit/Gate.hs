module Circuit.Gate where

data WireType = Bit | Qubit deriving (Show, Eq)

subscripts = "₀₁₂₃₄₅₆₇₈₉"
showSubscript :: Int -> String
showSubscript n | n >= 0 && n < 10 = [subscripts !! n]
showSubscript n = (showSubscript $ n `div` 10) ++ [subscripts !! (n `mod` 10)]

data Gate (sig :: [WireType]) where
    H :: Gate '[Qubit]
    X :: Gate '[Qubit]
    R :: Int -> Gate '[Qubit]
    --more later

instance Show (Gate sig) where
    show H = "H"
    show X = "X"
    show (R m) = "R" ++ (showSubscript m)

instance Eq (Gate sig) where
    H == H = True
    X == X = True
    (R m) == (R k) = m == k