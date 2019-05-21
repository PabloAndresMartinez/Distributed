module Distributer.Common where

import qualified Data.Map as M

import Quipper
import Quipper.Circuit

-- For each wire, a list of the hyperedges it 'controls'. Each (n,ws,m,b) is a hyperedge:
--   ws are the other vertices, pairs (wire,pos), the wire and the position of the CZ, 
--   (n,m) is the interval of indexes in [Gate] where it is located,
--   b==Control indicates the key is control (· dot), otherwise the key is target (+ dot)
type Hedge = (Int,[(Wire,Int)],Int)
type Hypergraph = M.Map Wire [Hedge]
type Block = Int
type Partition = M.Map Wire Block -- The wire 'w' is in the block given by: (partition M.! w). 

targetOf :: Gate -> Wire
targetOf gate = case gate of
  (QGate "X"   _ [target] [] _      _) -> target
  (QGate "Y"   _ [target] [] _      _) -> target
  (QGate "Z"   _ [target] [] _      _) -> target
  (QGate "S"   _ [target] [] _      _) -> target
  (QGate "T"   _ [target] [] _      _) -> target
  (QGate "H"   _ [target] [] _      _) -> target
  (QGate _     _ _        _  _      _) -> error standardError
  (QPrep target _)                     -> target
  (QUnprep target _)                   -> target
  (QInit _ target _)                   -> target
  (CInit _ target _)                   -> target
  (QTerm _ target _)                   -> target
  (CTerm _ target _)                   -> target
  (QMeas target)                       -> target
  (QDiscard target)                    -> target
  (CDiscard target)                    -> target
  _                                    -> error standardError
  where standardError = "Gate "++show gate++" was not properly prepocessed."

isCZ :: Gate -> Bool
isCZ (QGate "CZ" _ _ _ _ _) = True
isCZ _ = False

isClassical :: Gate -> Bool
isClassical (CNot _ _ _) = True -- This stands for Classical not, not the usual CNOT quantum gate
isClassical (CGate _ _ _ _) = True
isClassical (CGateInv _ _ _ _) = True
isClassical (CSwap _ _ _ _) = True
isClassical (CInit _ _ _) = True
isClassical (CTerm _ _ _) = True
isClassical (CDiscard _) = True
isClassical (DTerm _ _) = True
isClassical _ = False

getControls :: Gate -> [Signed Wire]
getControls (QGate _ _ _ [] ctrls _) = ctrls

getWires :: Gate -> (Wire,Wire)
getWires (QGate "CZ" _ [target] [] [signedCtrl] _) = (ctrl,target)
  where ctrl = from_signed signedCtrl

-- Notice that this number is always going to be less or equal than (length $ nonLocalCZs part hyp) because the latter counts "external" (those implemented in a QPU that is neither its control nor target) CZs twice
countNonLocal :: [Gate] -> [(Partition,Int)] -> Int
countNonLocal gates []              = 0
countNonLocal gates ((part,pos):ps) = nonLocal + countNonLocal nextGates ps
  where
    (thisGates,nextGates) = splitAt pos gates
    czs = map getWires $ filter isCZ thisGates
    nonLocal = length $ filter (\(w1,w2) -> part M.! w1 /= part M.! w2) czs