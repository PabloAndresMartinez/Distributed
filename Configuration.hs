module Distributer.Configuration where

epsilon = "0.01"

sea20 = "kahypar/config/km1_kKaHyPar_sea20.ini"

subalgorithm = sea20

{- List of available values for circuit:

-- From Quipper --
qft n -- where 'n' is the number of inputs (works fine up to 35)
bfWalk -- the quantum walk part of BooleanFormula, the other parts have a gate whose translation to Clifford+T is not supported by Quipper
bwt -- Binary Welded Tree
gse -- Ground State Estimation
usvR -- The algorithm first prepares a superposition of hypercubes, whose difference is the shortest vector. It then measures the output to collapse the state to a TwoPoint.

-- This one's partition is trivial:
usvH -- h_quantum from USV

-- The following should work, but take too much time due to the management of our data structures as lists (inefficient)
usvF -- f_quantum from USV
usvG -- g_quantum from USV

-- Additionally, these two don't work if bothRemotes is active, KaHyPart goes out of memory (more than 16GB)
tf  -- Triangle Finding problem 

-- Custom --
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

