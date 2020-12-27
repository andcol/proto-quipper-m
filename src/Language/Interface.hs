module Language.Interface where

import Interface

import Language.Core
import Language.Lift

type PQM circ exp = (HasLolli exp, HasCore exp, HasLift exp)