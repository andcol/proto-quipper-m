module Language.Interface where

import Interface

import Language.Core
import Language.Lift
import Language.Box

type PQM exp = (HasLolli exp, HasTensor exp, HasCore exp, HasLift exp, HasBox exp)