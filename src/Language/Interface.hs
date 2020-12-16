module Language.Interface where

import Interface

import Language.Core

type PQM exp = (HasLolli exp, HasCore exp)