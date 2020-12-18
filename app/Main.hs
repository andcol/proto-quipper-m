module Main where

import Lib
import Language.Interface

--DEBUG
import Circuit.Class
import Circuit.LabelContext
import Data.Proxy
import GHC.TypeLits

test :: Proxy ['(0,Qubit),'(1,Qubit),'(2,Qubit)]
test = Proxy

getLabels :: (KnownLabelContext lc) => Proxy lc -> Proxy (LabelsOf lc)
getLabels _ = Proxy

main :: IO ()
main = do
    let n = lablVal . getLabels $ test
    print n

