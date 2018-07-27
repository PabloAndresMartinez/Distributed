module Distributer.Configuration where

import Distributer.Examples

-- Partitioning parameters
k = "2"
epsilon = "0.03"

-- Set True to activate each extension, False to deactivate it
pullCNOTs = True
bothRemotes = True

-- The input circuit and its shape. Must be some of the cases from Examples.hs, listed below
circuit = gse

{- List of available values for circuit:

-- From Quipper --
qft n -- where 'n' is the number of inputs
bfWalk -- the quantum walk part of BooleanFormula, the other parts have a gate whose translation to Clifford+T is not supported by Quipper
bwt -- Binary Welded Tree
gse -- Ground State Estimation

-- Custom --
classical
classical2
subroutineCirc
simple
simple2
simple3
simple4
pull
pull1
pull2
interesting
interesting2
interesting3

-}