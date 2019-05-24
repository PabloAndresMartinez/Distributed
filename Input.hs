module Distributer.Input where

import Quipper

anExample = (anExample_C, (qubit,qubit,qubit,qubit))
anExample_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
anExample_C  (a,b,c,d) = do
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