module Mine.Examples where

import Quipper
import Quipper.Generic
import Quipper.Monad
import Quipper.QData
import Quipper.Circuit
import QuipperLib.Decompose
import QuipperLib.QFT
import Algorithms.BF.BooleanFormula
import System.Random
import Libraries.RandomSource
import QuipperLib.Unboxing

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

cancel :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
cancel (a,b) = do 
  qnot_at a `controlled` b
  qnot_at a `controlled` b
  return (a,b)