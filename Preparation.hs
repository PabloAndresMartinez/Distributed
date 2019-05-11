module Distributer.Preparation where

import qualified Data.Map as M
import System.Random

import Quipper
import Quipper.Generic
import Quipper.Circuit
import QuipperLib.Decompose
import QuipperLib.Unboxing
import Libraries.RandomSource

import qualified Distributer.Configuration as Cfg
import Distributer.Common

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
prepareCircuit :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> Mode -> (qin -> Circ qout)
prepareCircuit circ shape (f_pull,_) = circ4
  where
    circ1 = transform_generic swapRemover (unbox_recursive $ decompose_generic TrimControls (decompose_generic cliffordT circ)) -- Decompose to Clifford+T and inline all subroutines
    circ2 = \inp -> without_comments $ circ1 inp -- Ignore comments
    circ3 = separateClassicalControl circ2
    circ4 = if f_pull then pullCNOTs circ3 shape else circ3

-- Used to prevent the creation of hyperedges when the CNOT is only classically controlled
--   Separates X/not controlled gates into either X classically controlled or not quantumly controlled
--   The task of ignoring classical control is done by 'buildHyp' ignoring controlled X gates
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

-- Used when f_pull flag is active. Moves all CNOTs as much to the beginning as possible
pullCNOTs :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> (qin -> Circ qout)
pullCNOTs circ shape = unencapsulate_generic (x,((arin,theGates',arout,w),ns),y)
  where
    (x,((arin,theGates,arout,w),ns),y) = encapsulate_generic id circ shape
    theGates' = pullCNOTsRec theGates emptyDic
    emptyDic = M.fromList [(x,[]) | x <- [0..(w-1)]]

-- The second variable, 'past', holds the circuit up to the point that has been read, in reversed order. In 'pull', it has two components,
--   the first, npast (non-local past) is the earliest part of the circuit, which already has some CNOTs in it.
--   the second, lpast (local past) is the later parts comprised by 1-qubit gates, and maintained in a different list per wire.
pullCNOTsRec :: [Gate] -> M.Map Wire [Gate] -> [Gate]
pullCNOTsRec []     lpast = reverse $ (concat $ map (\(w,gs) -> gs) $ M.toList lpast)
pullCNOTsRec (g:gs) lpast = case g of
  (QGate "not" _ [target] [] [signedCtrl] _) -> (reverse npast') ++ (pullCNOTsRec gs $ M.insert ctrl cpast' $ M.insert target tpast' $ lpast)
    where
      ctrl = from_signed signedCtrl
      (npast', tpast', cpast') = pull target ctrl ([],lpast M.! target,lpast M.! ctrl)
      pull t c (np, tp, (p:cp)) = let 
        (np',tp',cp') = pull t c (np,tp,cp) -- The resulting past circuit after pushing the CNOT to the end, if it passed through 'p'
        standardError = "DistribHPartError: "++show g++" is not handled when pulling CNOTs."
        in case p of                         
          (QGate "not" _ _ [] _ _)       -> error standardError
          (QGate "X"   _ _ [] ctrls ncf) -> (np', (QGate "X" False [t] [] ctrls ncf):tp', p:cp') -- Pull through X, leaving an X byproduct
          (QGate "Z"   _ _ [] ctrls ncf) -> (np', tp', p:cp')                                    -- Pulls through directly
          (QGate "Y"   _ _ [] ctrls ncf) -> (np', (QGate "X" False [t] [] ctrls ncf):tp', p:cp') -- Pull through Y, leaving an X byproduct
          (QGate "S"   _ _ [] _ _)       -> (np', tp', p:cp')                                    -- Pulls through directly       
          (QGate "T"   _ _ [] _ _)       -> (np', tp', p:cp')                                    -- Pulls through directly       
          (QGate "H"   _ _ [] _ _)       -> pull t c (p:cp++np, tp, [])                            -- Can not be pulled through, flush the remaining gates
          (QGate _     _ _ _  _ _)       -> error standardError
          (QPrep _ _)                    -> pull t c (p:cp++np, tp, [])                            -- Can not be pulled through, flush the remaining gates
          (QInit _ _ _)                  -> pull t c (p:cp++np, tp, [])                            -- Can not be pulled through, flush the remaining gates
          _ -> error standardError
      pull t c (np, (p:tp), []) = let 
        (np',tp',cp') = pull t c (np,tp,[]) -- The resulting 'past' circuit after pushing the CNOT to the end, in case it passed through 'p'
        standardError = "DistribHPartError: "++show g++" is not handled when pulling CNOTs."
        in case p of                 
          (QGate "not" _ _ [] _ _)       -> error standardError
          (QGate "X"   _ _ [] ctrls ncf) -> (np', p:tp', cp')                                    -- Pull through directly
          (QGate "Z"   _ _ [] ctrls ncf) -> (np', p:tp', (QGate "Z" False [c] [] ctrls ncf):cp') -- Pull through Z, leaving a Z byproduct
          (QGate "Y"   _ _ [] ctrls ncf) -> (np', p:tp', (QGate "Z" False [c] [] ctrls ncf):cp') -- Pull through Y, leaving a Z byproduct
          (QGate "S"   _ _ [] _ _)       -> pull t c (p:tp++np, [], [])                            -- Can not be pulled through, flush the remaining gates
          (QGate "T"   _ _ [] _ _)       -> pull t c (p:tp++np, [], [])                            -- Can not be pulled through, flush the remaining gates
          (QGate "H"   _ _ [] _ _)       -> pull t c (p:tp++np, [], [])                            -- Can not be pulled through, flush the remaining gates
          (QGate _     _ _ _  _ _)       -> error standardError
          (QPrep _ _)                    -> pull t c (p:tp++np, [], [])                            -- Can not be pulled through, flush the remaining gates
          (QInit _ _ _)                  -> pull t c (p:tp++np, [], [])                            -- Can not be pulled through, flush the remaining gates
          _ -> error standardError     
      pull _ _ (np, [], []) = (g:np, [], [])   -- If it's a 1-qubit gate: 
  _ -> if Cfg.pullLimit < 0 || (length thisPast) < Cfg.pullLimit -- If pullLimit is infinite or larger than current memory,
          then pullCNOTsRec gs $ M.insert w thisPast lpast  -- simply add it to the local past
          else (reverse thisPast) ++ pullCNOTsRec gs (M.insert w [] lpast)  -- otherwise move the whole thing to the non-local past, if the memory limit is exceeded
    where 
      w = targetOf g
      thisPast = g:(lpast M.! w)