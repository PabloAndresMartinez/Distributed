module Distributer.Preparation where

import qualified Data.Map as M
import System.Random

import Quipper
import Quipper.Generic
import Quipper.Circuit
import QuipperLib.Decompose
import QuipperLib.Decompose.CliffordT
import QuipperLib.Unboxing

import qualified Distributer.Configuration as Cfg
import Distributer.Common

prepareCircuit ::  (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> (qin -> Circ qout)
prepareCircuit circ shape = pushSingleQGates (step5 $ step4 $ step3 $ step2 $ step1 circ) shape
  where
    step1 = unbox_recursive -- Inline all subroutines
    step2 = if Cfg.keepToffoli then decompose_generic Toffoli else id
    step3 = transform_generic standard_transformer' . transform_generic exact_ct_transformer' . transform_generic approx_ct_transformer' -- Essentially, decompose_generic Standard, but ignoring Toffoli gates if f_toffoli is True
    step4 = transform_generic separateClassicalControl . transform_generic swapRemover
    step5 = transform_generic toControlledZ

-- The following three transformers are a version of the ones defined by Quipper, but ignoring Toffoli gates if the flag is True.
approx_ct_transformer' :: Transformer Circ Qubit Bit
approx_ct_transformer' g@(T_QGate "not" 1 0 _ _ _) = if Cfg.keepToffoli then identity_transformer g else approx_ct_transformer False (3*digits) (mkStdGen 1234) g
approx_ct_transformer' g                           = approx_ct_transformer False (3*digits) (mkStdGen 1234) g

exact_ct_transformer' :: Transformer Circ Qubit Bit
exact_ct_transformer' g@(T_QGate "not" 1 0 _ _ _) = if Cfg.keepToffoli then identity_transformer g else exact_ct_transformer g
exact_ct_transformer' g                           = exact_ct_transformer g

standard_transformer' ::Transformer Circ Qubit Bit
standard_transformer' g@(T_QGate "not" 1 0 _ _ _) = if Cfg.keepToffoli then identity_transformer g else standard_transformer g
standard_transformer' g                           = standard_transformer g

-- Gate decomposition from Quipper does not decompose SWAP into CNOT gates
swapRemover :: Transformer Circ Qubit Bit
swapRemover (T_QGate "swap" 2 0 _ ncf f) = f $
  \[q0, q1] [] ctrls -> without_controls_if ncf $ do
    with_controls ctrls $ do
      qnot_at q0 `controlled` q1
      qnot_at q1 `controlled` q0
      qnot_at q0 `controlled` q1
      return ([q0, q1], [], ctrls)
swapRemover g = identity_transformer g

-- Used to prevent the creation of hyperedges when the 'not' gate is only classically controlled
--   Separates X/not controlled gates into either X classically controlled or 'not', which are quantumly controlled
--   The task of ignoring classical control is done by 'buildHyp' ignoring controlled X gates
separateClassicalControl :: Transformer Circ Qubit Bit
separateClassicalControl g@(T_QGate gate 1 0 _ ncf f) 
  | gate == "not" || gate == "X" = f $
    \[target] [] ctrls -> without_controls_if ncf $ do
      with_controls ctrls $ let 
        classicalCtrls = foldr (&&) True $ map (isClassical . from_signed) ctrls
        isClassical e = case e of (Endpoint_Qubit _) -> False; (Endpoint_Bit _) -> True
        in do
          if null ctrls then gate_X_at target else
            if classicalCtrls then gate_X_at target `controlled` ctrls else qnot_at target `controlled` ctrls 
          return ([target], [], ctrls) 
  | otherwise = identity_transformer g
separateClassicalControl g = identity_transformer g

-- Used to convert controlled not gates into controlled Z gates, whose wires all act in the same way (so it's easier to distribute them)
toControlledZ :: Transformer Circ Qubit Bit
toControlledZ g@(T_QGate "not" 1 0 _ _ f) = f $
  \[target] [] ctrls -> do
    gate_H_at target
    named_gate_at "CZ" target `controlled` ctrls -- A different name so we differentiate these from Pauli Z gates, which may have classical control
    gate_H_at target
    return ([target], [], ctrls)
toControlledZ g = identity_transformer g

pushSingleQGates :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> (qin -> Circ qout)
pushSingleQGates circ shape = unencapsulate_generic (x,((arin,theGates',arout,w),ns),y)
  where
    (x,((arin,theGates,arout,w),ns),y) = encapsulate_generic id circ shape
    theGates' = pushRec theGates $ M.fromList [(x,[]) | x <- [0..(w-1)]]

-- We must beware of possible classical controls of the gates. Quantum controls may only appear in CZ, thanks to previous preprocessing
pushRec :: [Gate] -> M.Map Wire [Gate] -> [Gate]
pushRec []     past = foldr (\(_,gs) l -> reverse gs ++ l) [] $ M.toList past
pushRec (g:gs) past = if isClassical g then g : pushRec gs past -- Just append: it acts on a classical wire, so it doesn't affect our algorithm
  else case g of 
    (QGate "CZ" _ [w] [] cs _) -> flushed ++ g : pushRec gs pushedPast
      where
        flushed = concat $ map (\(p,wPast) -> reverse $ dropWhile isNotH wPast) wirePasts
        pushedPast = foldr (\(p, wPast) past' -> M.insert p (addByproducts wPast ctrls) past') past toPush -- Inserting overwrites that wire's values
        toPush = map (\(p,wPast) -> (p, takeWhile isNotH wPast)) wirePasts
        isNotH gate = not $ "H" == nameOf gate
        wirePasts = map (\p -> (p, past M.! p)) (w:ctrls)
        ctrls = map from_signed cs
    (QGate "Z"  _ [w] [] cs ncf) -> if (not $ null $ past M.! w) && "H" == nameOf (getHeadAt w) && (null $ getControls (getHeadAt w))
      then pushRec gs (appendToDic w (getHeadAt w) $ appendToDic w (gateX w cs ncf) $ tailFrom w past) -- Flip with Hadamard and append
      else pushRec gs (appendToDic w g past)                                                           -- Just add to past
    (QGate "X"  _ [w] [] cs ncf) -> if (not $ null $ past M.! w) && "H" == nameOf (getHeadAt w) && (null $ getControls (getHeadAt w)) 
      then pushRec gs (appendToDic w (getHeadAt w) $ appendToDic w (gateZ w cs ncf) $ tailFrom w past) -- Flip with Hadamard and append
      else pushRec gs (appendToDic w g past)                                                           -- Just add to past
    (QGate "Y"  _ [w] [] cs ncf) -> if (not $ null $ past M.! w) && "H" == nameOf (getHeadAt w) && (null $ getControls (getHeadAt w))
      then pushRec gs (appendToDic w (getHeadAt w) $ appendToDic w (gateZ w cs ncf) $ appendToDic w (gateX w cs ncf) $ tailFrom w past) 
      else pushRec gs (appendToDic w (gateX w cs ncf) $ appendToDic w (gateZ w cs ncf) past)           -- Y = iXZ, so same as having Z then X
    (QGate "H"  _ [w] [] cs ncf) -> if (not $ null $ past M.! w) && "H" == nameOf (getHeadAt w) && null cs && (null $ getControls (getHeadAt w)) 
      then pushRec gs (tailFrom w past)                             -- Hadamards are cancelled
      else pushRec gs (appendToDic w g past)                        -- Add the gate to the past
    (QGate "S"  _ [w] [] _ _)  -> pushRec gs (appendToDic w g past) -- Just add to past
    (QGate "T"  _ [w] [] _ _)  -> pushRec gs (appendToDic w g past) -- Just add to past
    (QGate _    _ _   _  _ _)  -> error standardError               -- Not recognised
    (QUnprep w _)              -> gatesAt w past ++ g : pushRec gs (flushAt w past) -- Append and flush
    (QMeas w)                  -> gatesAt w past ++ g : pushRec gs (flushAt w past) -- Append and flush
    (QDiscard w)               -> gatesAt w past ++ g : pushRec gs (flushAt w past) -- Append and flush
    (QTerm _ w _)              -> gatesAt w past ++ g : pushRec gs (flushAt w past) -- Append and flush
    (QPrep _ _)                -> g : pushRec gs past -- Just append
    (QInit _ _ _)              -> g : pushRec gs past -- Just append
    (Comment _ _ _)            -> pushRec gs past     -- Just ignore
  where
    appendToDic w gate dic = M.alter (\(Just l) -> Just $ gate:l) w dic
    tailFrom    w      dic = M.alter (\(Just l) -> Just $ tail l) w dic
    flushAt     w      dic = M.alter (\_        -> Just []) w dic
    getHeadAt w = head $ past M.! w
    gatesAt w dic = (reverse $ dic M.! w)
    gateZ w cs ncf = QGate "Z"  False [w] [] cs ncf
    gateX w cs ncf = QGate "X"  False [w] [] cs ncf
    nameOf (QGate name _ _ _ _ _) = name 
    standardError = "Gate "++show g++" is not handled when pushing single qubit gates forward."

addByproducts :: [Gate] -> [Wire] -> [Gate]
addByproducts [] _ = []
addByproducts (g@(QGate "X" _ _ [] cs ncf):gs) wires = map (\w -> QGate "Z" False [w] [] cs ncf) wires ++ g : addByproducts gs wires
addByproducts (g:gs) wires = g : addByproducts gs wires
