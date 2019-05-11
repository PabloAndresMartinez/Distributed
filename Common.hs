module Distributer.Common where

import qualified Data.Map as M

import Quipper
import Quipper.Circuit

type Mode = (Bool,Bool) -- (PullCNOTs,BothRemotes)
-- For each wire, a list of the hyperedges it 'controls'. Each (n,ws,m,b) is a hyperedge:
--   ws are the other vertices, pairs (wire,pos), the wire and the position of the CNOT, 
--   (n,m) is the interval of indexes in [Gate] where it is located,
--   b==Control indicates the key is control (· dot), otherwise the key is target (+ dot)
type Hypergraph = M.Map Wire [(Int,[(Wire,Int)],Int,HedgeType)]
data HedgeType = Control | Target | Unknown deriving (Eq, Ord, Show)
type Block = Int
type Partition = M.Map Wire Block -- The wire 'w' is in the block given by: (partition M.! w). 

targetOf :: Gate -> Wire
targetOf gate = case gate of
  (QGate "not" _ [target] [] _      _) -> error standardError 
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
  where standardError = "DistribHPartError: Gate "++show gate++" was not properly prepocessed."

isCNOT :: Gate -> Bool
isCNOT (QGate "not" _ _ _ _ _) = True
isCNOT _ = False

getWires :: Gate -> (Wire,Wire)
getWires (QGate "not" _ [target] [] [signedCtrl] _) = (ctrl,target)
  where ctrl = from_signed signedCtrl

-- Notice that this number is always going to be less or equal than (length $ nonLocalCNOTs part hyp) because the latter counts "external" (those implemented in a QPU that is neither its control nor target) CNOTs twice
countNonLocal :: [Gate] -> Partition -> Int
countNonLocal gates partition = nonLocal
  where
    cnots = map getWires $ filter isCNOT gates :: [(Wire,Wire)]
    nonLocal = length $ filter (\(p1,p2) -> p1/=p2) $ allocate cnots
      where
        allocate []     = []
        allocate ((w1,w2):cs) = (partition M.! w1, partition M.! w2) : allocate cs