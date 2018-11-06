module Distributer.Configuration where

import Distributer.Examples
import Quipper.Printing

-- Path to KaHyPar program
kahyparDir = "../../../../"

-- Partitioning parameters
k = "15"
epsilon = "0.03"

-- Set True to activate each extension, False to deactivate it
pullCNOTs = True
bothRemotes = True

-- A limit to avoid going out of memory in circuits with a lot of consecutive 1-qubit gates (usually product of approximating a non-Clifford gate)
pullLimit :: Int 
pullLimit = 5 -- Set to -1 for infinite (i.e. no limit)

-- The input circuit and its shape. Must be some of the cases from Examples.hs, listed below
circuit = qft 200
-- Show output (either Preview, to see the circuit, or GateCount, to see the stats):
outputAs = GateCount

{- List of available values for circuit:

-- From Quipper --
qft n -- where 'n' is the number of inputs (works fine up to 35)
bfWalk -- the quantum walk part of BooleanFormula, the other parts have a gate whose translation to Clifford+T is not supported by Quipper
bwt -- Binary Welded Tree
gse -- Ground State Estimation

-- This one's partition is trivial:
usvH -- h_quantum from USV

-- The following should work, but take too much time due to the management of our data structures as lists (inefficient)
usvF -- f_quantum from USV
usvG -- g_quantum from USV

-- Additionally, these two don't work if bothRemotes is active, KaHyPart goes out of memory (more than 16GB)
tf  -- Triangle Finding problem 
usvR -- The algorithm first prepares a superposition of hypercubes, whose difference is the shortest vector. It then measures the output to collapse the state to a TwoPoint.

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