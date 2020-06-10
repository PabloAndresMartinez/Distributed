module Distributer.Common where

import qualified Data.Map as M
import Data.List (nub)

import Quipper
import Quipper.Internal.Circuit

-- Parameters
type K = Int
type Size = Int
type InitSegSize = Int
type MaxHedgeDist = Int
type KeepCCZ = Bool
data PartAlg = Kahypar | Patoh
type PartDir = String
type SaveTrace = Bool
type Verbose = Bool

-- For each wire, a list of the hyperedges it 'controls'. Each (n,ws,m,b) is a hyperedge:
--   ws are the other vertices, pairs (wire,pos), the wire and the position of the CZ, 
--   (n,m) is the interval of indexes in [Gate] where it is located,
--   b==Control indicates the key is control (· dot), otherwise the key is target (+ dot)
type Hedge = (Int,[(Wire,Int)],Int)
type Hypergraph = M.Map Wire [Hedge]
type Block = Int
type Partition = M.Map Wire Block -- The wire 'w' is in the block given by: (partition M.! w). 
type Matching   = M.Map Block Block

-- The segment's gates, its hypergraph, its partition and the 'seam' cost that matches it with the next segment, the last element is the identifier
type Segment = ([Gate], Hypergraph, Partition, Seam, (Int,Int))
data Seam = Compute | Value Rational | Stop

seamOf :: Segment -> Seam
seamOf (_,_,_,seam,_) = seam

isStop :: Seam -> Bool
isStop Stop = True
isStop _    = False

updWith :: Matching -> Segment -> Segment
updWith matching (gs,hyp,part,seam,id) = (gs, hyp, M.map (\b -> matching M.! b) part, seam, id)

targetOf :: Gate -> Maybe Wire
targetOf gate = case gate of
  (QGate "X"   _ [target] [] _      _) -> Just target
  (QGate "Y"   _ [target] [] _      _) -> Just target
  (QGate "Z"   _ [target] [] _      _) -> Just target
  (QGate "S"   _ [target] [] _      _) -> Just target
  (QGate "T"   _ [target] [] _      _) -> Just target
  (QGate "H"   _ [target] [] _      _) -> Just target
  (QGate _     _ _        _  _      _) -> Nothing
  (QPrep target _)                     -> Just target
  (QUnprep target _)                   -> Just target
  (QInit _ target _)                   -> Just target
  (CInit _ target _)                   -> Just target
  (QTerm _ target _)                   -> Just target
  (CTerm _ target _)                   -> Just target
  (QMeas target)                       -> Just target
  (QDiscard target)                    -> Just target
  (CDiscard target)                    -> Just target
  _                                    -> Nothing

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

getWires :: Gate -> [Wire]
getWires (QGate "CZ" _ [target] [] signedCtrls _) = target : ctrls
  where ctrls = map from_signed signedCtrls

-- Notice that this number is always going to be less or equal than (length $ nonLocalCs part hyp) because the latter counts "external" (those implemented in a QPU that is neither its control nor target) CZs twice
countNonLocal :: [Segment] -> Int
countNonLocal []     = 0
countNonLocal (s:ss) = nonLocal + countNonLocal ss
  where
    nonLocal = sum $ map (\ws -> length ws - 1) $ map (\ws -> nub $ map (thisPart M.!) ws) czs
    czs = map getWires $ filter isCZ thisGates
    (thisGates,_,thisPart,_,_) = s