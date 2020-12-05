module Circuit where

import Data.Either

type QId = Int

data Gate = H | X --more later

class LabelledCircuit c where
    labels :: c -> [QId]
    inputs :: c -> [QId]
    outputs :: c -> [QId]
    fromGate :: Gate -> c
    substituteLabel :: QId -> QId -> c -> c
    append :: c -> [QId] -> c -> (c, [QId])

data LabelledGate = LGate QId Gate QId
type NaiveCircuit = [LabelledGate]

instance LabelledCircuit NaiveCircuit where
    labels [] = []
    labels ((LGate l _ l'):cs) = [l,l'] ++ (labels cs)
    
    inputs [] = []
    inputs ((LGate l _ _):cs) = l:(inputs cs)
    
    outputs [] = []
    outputs ((LGate _ _ l'):cs) = l':(outputs cs)
    
    fromGate H = [LGate 0 H 1]
    fromGate X = [LGate 0 X 1]

    substituteLabel _ _ [] = []
    substituteLabel k k' ((LGate l g l'):cs) | k == l = (LGate k' g l'):(substituteLabel k k' cs)
    substituteLabel k k' ((LGate l g l'):cs) | k == l' = (LGate l g k'):(substituteLabel k k' cs)
    substituteLabel k k' ((LGate l g l'):cs) = (LGate l g l'):(substituteLabel k k' cs)
    
    append c k d = let
        lastLabel = maximum $ labels c
        freshRequired = length $ outputs d
        freshLabels = map (lastLabel +) [1..freshRequired]
        d' = foldr (uncurry substituteLabel) d (zip (outputs d) freshLabels)
        d'' = foldr (uncurry substituteLabel) d' (zip (inputs d) k)
        c' = c ++ d''
        in (c', freshLabels)
        