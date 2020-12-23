module Language.Interface where

import Interface

import Language.Core

type PQM circ exp = (HasLolli exp, HasCore exp)