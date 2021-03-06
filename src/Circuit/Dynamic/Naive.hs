module Circuit.Dynamic.Naive where

import Circuit.Gate
import Circuit.Dynamic.Class
import Data.List
import Control.Monad

data Circuit = Identity LabelContext | forall sig. Circuit :+ (LabelContext, Gate sig, LabelContext) 

showlc :: LabelContext -> String
showlc [(l,_)] = show l
showlc ((l,_):r) = (show l) ++ ", " ++ (showlc r)

instance Show Circuit where
    show c = "Input wires: " ++ (showlc $ inputsOf c) ++ "\n" ++ (show' c) ++ "Output wires: " ++ (showlc $ outputsOf c)
                where
            show' (Identity lc) = ""
            show' (c :+ (lc1,g,lc2)) =  (show' c) ++ (showlc lc2) ++ " ← " ++ (show g) ++ " " ++ (showlc lc1) ++ "\n"

latestLabel :: Circuit -> Label
latestLabel (Identity lc) = let (labels,_) = unzip lc in maximum labels
latestLabel (c :+ (lc1, _, lc2)) = let
    max = latestLabel c
    max1 = let (labels1,_) = unzip lc1 in maximum labels1
    max2 = let (labels2,_) = unzip lc2 in maximum labels2
    in
        maximum [max,max1,max2]

freshLabels :: Circuit -> Int -> [Label]
freshLabels c n = let first = (latestLabel c) + 1 in [first + i | i <- [0..n-1]]

freshInAll :: [Circuit] -> Int -> [Label]
freshInAll circList n = let first = (maximum $ map latestLabel circList) + 1 in [first + i | i <- [0..n-1]]

substituteInContext :: [(Label, Label)] -> LabelContext -> LabelContext
substituteInContext subs [] = []
substituteInContext [] lc = lc
substituteInContext subs ((l,w):rc) = let
    newBinding = case lookup l subs of
        Nothing -> (l,w)
        Just k -> (k,w)
    in
        newBinding : (substituteInContext subs rc)

substituteLabels :: [(Label, Label)] -> Circuit -> Circuit
substituteLabels subs (Identity lc) = Identity (substituteInContext subs lc)
substituteLabels subs (c :+ (lc1, g, lc2)) = (substituteLabels subs c) :+ (substituteInContext subs lc1, g, substituteInContext subs lc2)

outputsOf :: Circuit -> LabelContext
outputsOf c = outputsOf' c [] [] where
    outputsOf' (Identity lc) consumed tentative = (lc \\ consumed) ++ tentative --there should be no duplicates, but check maybe
    outputsOf' (c :+ (lc1, _, lc2)) consumed tentative = outputsOf' c (consumed ++ lc1) ((lc2 ++ tentative) \\ consumed) --check

inputsOf :: Circuit -> LabelContext
inputsOf (Identity lc) = lc
inputsOf (c :+ _) = inputsOf c

match :: Circuit -> [Label] -> Circuit -> Bool
match c [] d = True
match c (k:lr) d = let
    lc1 = outputsOf c
    ((l,w2):rc2) = inputsOf d
    outcome = lookup k lc1
    in case outcome of
        Just w1 | w1 == w2 -> True
        _ -> False


maybeAppend :: Circuit -> [Label] -> Circuit -> Maybe (Circuit, [Label])
maybeAppend c targets d = do
    when (not (match c targets d)) Nothing --fail immediately if c and d are incompatible
    let (l, l') = (labelsOf $ inputsOf d, labelsOf $ outputsOf d)
    let k' = freshInAll [c,d] (length l')
    let d' = substituteLabels (zip (l ++ l') (targets ++ k')) d
    return (c `concat` d', k')
    where
        c `concat` (Identity _) = c
        c `concat` (d' :+ gate) = (c `concat` d') :+ gate

instance LabelledCircuit Circuit where
    identity = Identity
    
    fromGate Meas = (Identity [(0,Qubit)]) :+ ([(0,Qubit)], Meas, [(1,Qubit)]) -- For now, Meas outputs a qubit (bit is still unimplemented)
    fromGate H = (Identity [(0,Qubit)]) :+ ([(0,Qubit)], H, [(1,Qubit)])
    fromGate X = (Identity [(0,Qubit)]) :+ ([(0,Qubit)], X, [(1,Qubit)])
    fromGate Z = (Identity [(0,Qubit)]) :+ ([(0,Qubit)], Z, [(1,Qubit)])
    fromGate (R m) = (Identity [(0,Qubit)]) :+ ([(0,Qubit)], (R m), [(1,Qubit)])
    fromGate (C g) = let    (Identity _) :+ (lci, _, lco) = fromGate g --this case is so messy, it could be refactored
                            lci' = (0,Qubit) : (shift 1 lci)
                            fstlco = let (i,_):_ = lco in i
                            fstlco' = (maxindex lci') + 1
                            lco' = (fstlco',Qubit) : (shift (fstlco' - fstlco + 1) lco) in
                                (Identity lci') :+ (lci', (C g), (lco'))
                                where
                                shift _ [] = []
                                shift n ((i,x):r) = (i+n,x):(shift n r)
                                maxindex [(i,_)] = i
                                maxindex ((i,_):r) = max i (maxindex r)

    append c targets d = case maybeAppend c targets d of
        Just x -> x
        Nothing -> error "application in which some targets do not exist or are of the wrong type" --not very Haskell of me

parallel :: Gate sig -> Int -> Circuit
parallel g 1 = fromGate g
parallel g n = fst $ append (Identity (zip [0..n-1] (replicate n Qubit)) :+ ([(0,Qubit)], g, [(n,Qubit)])) [1..n-1] (parallel g (n-1))

hadamard :: Int -> Circuit
hadamard = parallel H

measure :: Int -> Circuit
measure = parallel Meas