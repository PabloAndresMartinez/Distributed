module Distributer.Examples where

import Quipper
import Quipper.Generic
import Quipper.Monad
import Quipper.QData
import Quipper.Circuit
import QuipperLib.Decompose
import Algorithms.BF.BooleanFormula
import System.Random
import Libraries.RandomSource
import QuipperLib.Unboxing

import QuipperLib.QFT
import qualified Algorithms.BF.BooleanFormula as BF
import qualified Algorithms.BWT.BWT as BWT
import qualified Algorithms.GSE.GSE as GSE
import qualified Algorithms.GSE.GSEData as GSEData

and_gate :: Circ (Qubit)
and_gate = do
  a <- qinit False
  b <- qinit False
  c <- qinit False
  hadamard_at c
  gate_Z_at c `controlled` [a, b]
  hadamard_at c
  return c

classical :: (Bit, Qubit) -> Circ (Qubit, Qubit)
classical (b,q) = do
  qnot_at q `controlled` b
  r <- prepare b
  swap_at r q
  return (r,q)

classical2 :: (Bit, Bit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit)
classical2 (b1,b2,q1,q2,q3) = do
  qnot_at q1 `controlled` [b1,b2]
  qnot_at q1 `controlled` [q2,q3]
  r1 <- prepare b1
  r2 <- prepare b2
  return (r1,r2,q1,q2,q3)

rep :: Integer
rep = 3

myfunc :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
myfunc (a,b) = do
  hadamard_at a `controlled` b
  return (b,a)

subroutineCirc :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit)
subroutineCirc (a,b,c,d,e,f) = do
  label (a,b,c,d,e,f) ("a","b","c","d","e","f")
  (a,b) <- loopM rep (a,b) myfunc
  (c,d) <- box_loopM "box1" rep (c,d) myfunc
  (e,f) <- unbox (\x -> box_loopM "box1" rep x myfunc) (e,f)
  label (a,b,c,d,e,f) ("a","b","c","d","e","f") 
  return (a,b,c,d,e,f)

basic :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
basic (a,b) = do 
  qnot_at a `controlled` b
  return (a,b)

simple :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
simple (a,b,c,d) = do
  qnot_at d `controlled` b
  qnot_at c `controlled` b
  qnot_at a `controlled` b
  return (a,b,c,d)

simple2 :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
simple2 (a,b,c,d) = do
  qnot_at d `controlled` b
  qnot_at c `controlled` b
  qnot_at a `controlled` b
  qnot_at b `controlled` a
  qnot_at c `controlled` a
  qnot_at d `controlled` a
  return (a,b,c,d)

simple3 :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
simple3 (a,b,c,d) = do
  qnot_at a `controlled` b
  hadamard_at b
  hadamard_at a
  qnot_at a `controlled` b
  hadamard_at b
  hadamard_at a
  qnot_at a `controlled` b
  hadamard_at b
  hadamard_at a
  qnot_at b `controlled` c
  hadamard_at c
  hadamard_at d
  qnot_at d `controlled` c
  hadamard_at c
  hadamard_at d
  qnot_at d `controlled` c
  return (a,b,c,d)

simple4 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
simple4 (a,b,c) = do
  qnot_at b `controlled` a
  qnot_at c `controlled` a
  qnot_at c `controlled` b  
  return (a,b,c)

pull :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
pull (a,b) = do
  qnot_at b `controlled` a
  named_gate_at "X" a
  named_gate_at "Z" b
  qnot_at b `controlled` a
  named_gate_at "X" a
  named_gate_at "Z" b
  qnot_at a `controlled` b
  named_gate_at "X" a
  named_gate_at "Z" b
  return (a,b)

pull2 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
pull2 (a,b,c) = do
  qnot_at a `controlled` b
  gate_X_at a
  qnot_at b `controlled` a
  gate_H_at b
  qnot_at a `controlled` b
  gate_Y_at b
  qnot_at b `controlled` c
  gate_H_at b
  qnot_at c `controlled` b
  gate_Z_at c
  qnot_at b `controlled` c
  return (a,b,c)

pull3 :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
pull3 (a,b,c) = do
  qnot_at b `controlled` c 
  gate_X_at b
  qnot_at b `controlled` a
  gate_Y_at b
  qnot_at a `controlled` c
  gate_H_at a
  gate_T_at c
  qnot_at b `controlled` c
  qnot_at a `controlled` b
  return (a,b,c)

interesting :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting (a,b,c,d) = do
  qnot_at d `controlled` a
  qnot_at b `controlled` c
  qnot_at d `controlled` c
  gate_T_at b
  gate_H_at c
  qnot_at b `controlled` a
  qnot_at a `controlled` c
  return (a,b,c,d)

interesting2 :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting2  (a,b,c,d) = do
  qnot_at d `controlled` a
  gate_T_at a
  qnot_at c `controlled` a
  qnot_at a `controlled` b
  gate_H_at b
  qnot_at c `controlled` d
  qnot_at d `controlled` b
  qnot_at a `controlled` b
  gate_H_at b
  gate_Y_at d
  qnot_at d `controlled` b
  return (a,b,c,d)

interesting3 :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting3 (a,b,c,d) = do
  qnot_at d `controlled` a
  gate_S_at a
  gate_Y_at d
  qnot_at b `controlled` a 
  qnot_at d `controlled` c
  gate_H_at a
  qnot_at d `controlled` b
  qnot_at c `controlled` a
  gate_Z_at a
  qnot_at d `controlled` a
  return (a,b,c,d)

-- BF, only the walk part, using the default test oracle
bf = BF.walk

bf_shape = registerShape $ test_oracle

-- BWT with default options
bwt :: () -> Circ [Bit]
bwt _ = BWT.qrwbwt oracle s dt
  where
    s = 1 -- number of timesteps
    dt = pi/180 -- timestep size
    oracle = BWT.oracle_orthodox f g -- the default oracle construction for the two given bitstrings
      where
        n = 5 -- input bitstring length
        f = take n (True : False : f) -- a function
        g = take n (False : True : g) -- another function
        
-- GSE overall circuit with default options
--    Needs to be IO because it loads from some files    
gse :: IO (() -> Circ [Bit])
gse = let 
  b        = 3 -- The number of precision qubits
  m        = 4 -- The number of basis functions
  occupied = 2 -- The number of occupied orbitals
  delta_e  = 6.5536 -- Energy range
  tau = 2*pi / delta_e -- The Hamiltonian scaling parameter
  e_max    = -3876.941 -- Maximum energy
  nfun     = (\k -> 1)  -- The function k -> Nk; by default, we skip the repetition
  orthodox = False -- Use orthodox Coulomb operator?
  -- Integral data hpq and hpqrs:
  h1_file  = "h_1e_ascii"
  h2_file  = "h_2e_ascii"
  datadir  = "gseData/"
  in do
    gse_data <- GSEData.load_gse_data m (datadir++h1_file) (datadir++h2_file)
    return $ (\_ -> GSE.gse b m occupied gse_data tau e_max nfun orthodox)
    
