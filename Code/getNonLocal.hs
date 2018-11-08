import Quipper
import Quipper.Circuit
import Quipper.Generic
import QuipperLib.Decompose
import QuipperLib.Unboxing
import Distributer.Examples
import Libraries.RandomSource

import System.Random
import System.Environment
import Data.List (sortBy, sort)
import qualified Data.Map as M


cliffordT :: GateBase
cliffordT = Standard (3*digits) (RandomSource $ mkStdGen 1234)

-- Gate decomposition from Quipper does not decompose SWAP into CNOT gates
swapRemover :: Transformer Circ Qubit Bit
swapRemover (T_QGate "swap" 2 0 _ ncf f) = f $
  \[q0, q1] [] ctrls -> do
    without_controls_if ncf $ do
      with_controls ctrls $ do
        qnot_at q0 `controlled` q1
        qnot_at q1 `controlled` q0
        qnot_at q0 `controlled` q1
        return ([q0, q1], [], ctrls)
swapRemover g = identity_transformer g


-- We need to transform the circuit into single qubit gates or two-qubit gates where one acts as control. The easiest is go to {CNOT,Rot}.
--   If the boolean flag is True, Pauli gates are pushed through CNOTs
prepareCircuit :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> (qin -> Circ qout)
prepareCircuit circ shape = circ3
  where
    circ1   = transform_generic swapRemover (unbox_recursive $ decompose_generic TrimControls (decompose_generic cliffordT circ)) -- Decompose to Clifford+T and inline all subroutines *PROVISIONAL* 
    circ2  = \inp -> without_comments $ circ1 inp -- Ignore comments
    circ3 = separateClassicalControl circ2

-- Used to prevent the creation of hyperedges when the CNOT is only classically controlled
--   Separates X/not controlled gates into either X classically controlled or not quantumly controlled
--   The task of ignoring classical control is done by the 'buildHyp' taking this into account
separateClassicalControl :: (QCData qin, QCData qout) => (qin -> Circ qout) -> (qin -> Circ qout)
separateClassicalControl circ = transform_generic separator circ
  where
    separator g@(T_QGate gate 1 0 _ ncf f) 
      | gate == "not" || gate == "X" = f $
        \[target] [] ctrls -> do
          without_controls_if ncf $ do
            with_controls ctrls $ let 
              quantumCtrls = (isQuantum . from_signed . head) ctrls
              isQuantum e = case e of (Endpoint_Qubit _) -> True; (Endpoint_Bit _) -> False
              in do
                if null ctrls then gate_X_at target else
                  if quantumCtrls then qnot_at target `controlled` ctrls else gate_X_at target `controlled` ctrls
                return ([target], [], ctrls) 
      | otherwise = identity_transformer g
    separator g   = identity_transformer g


isCNOT :: Gate -> Bool
isCNOT (QGate "not" _ _ _ _ _) = True
isCNOT _ = False

getWires :: Gate -> (Wire,Wire)
getWires (QGate "not" _ [target] [] [signedCtrl] _) = (ctrl,target)
  where ctrl = from_signed signedCtrl

getNonLocal :: [Gate] -> [Int] -> Int
getNonLocal gates partition = nonLocal
  where
    partDic = M.fromList $ zip [0..] partition
    cnots = map getWires $ filter isCNOT gates :: [(Wire,Wire)]
    nonLocal = length $ filter (\(p1,p2) -> p1/=p2) $ allocate cnots
      where
        allocate []     = []
        allocate ((w1,w2):cs) = (partDic M.! w1, partDic M.! w2) : allocate cs

main = do
  [path] <- getArgs
  putStrLn path
  (circ,shape,_) <- qft 200
  partFile <- readFile $ path++"partition.hgr"
  let
    (_,((_,theGates,_,nWires),_),_) = encapsulate_generic id (prepareCircuit circ shape) shape
    partition = map read $ lines partFile :: [Int]
    in appendFile (path++"output.txt") $ "\nNumber of non-local CNOTs: " ++ (show $ getNonLocal theGates partition)

