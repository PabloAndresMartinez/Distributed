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

import qualified Data.Map as M
import Data.List (sortBy, (!!))
import qualified HSH as HSH
import Mine.Examples

type Mode = (Bool,Bool,Bool) -- (PullCNOTs,BothEnds,JoinEbits)

-- ## Auxiliary functions ## --
rmdups :: (Eq a) => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = if x `elem` xs' then xs' else (x:xs')
  where
    xs' = rmdups xs

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
  where standardError = "DistribHPartError: Gate "++show gate++"was not properly prepocessed."

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

{-
-- Given a partition, and a circuit with the required ebits already initialised, inject the necessary gates to achieve the distributed circuit
gateInjections :: Partition -> Transformer Circ Qubit Bit
gateInjections (partition, eDic) (T_QGate "not" 1 0 _ ncf f) = f $
  \[target] [] [ctrlEndpoint] -> do
    without_controls_if ncf $ let 
      bt = partition !! wire_of_qubit target; (Endpoint_Qubit ctrl) = from_signed ctrlEndpoint; bc = partition !! wire_of_qubit ctrl in 
        if bt == bc then do -- Internal
          qnot_at target `controlled` ctrl
          return ([target], [], [ctrlEndpoint])
        else do -- Inject using the assigned ebit
          qnot_at target `controlled` (eDic M.! (wire_of_qubit ctrl,bt))
          return ([target], [], [ctrlEndpoint])
gateInjections _ g = identity_transformer g
-}

-- We need to transform the circuit into single qubit gates or two-qubit gates where one acts as control. The easiest is go to {CNOT,Rot}.
--   If the boolean flag is True, Pauli gates are pushed through CNOTs
prepareCircuit :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> Mode -> (qin -> Circ qout)
prepareCircuit circ shape (f_pull,_,_) = circ4
  where
    circ1   = unbox_recursive (transform_generic swapRemover $ decompose_generic TrimControls (decompose_generic cliffordT circ)) -- Decompose to Clifford+T and inline all subroutines *PROVISIONAL* 
    circ2  = \inp -> without_comments $ circ1 inp -- Ignore comments
    circ3 = separateClassicalControl circ2
    circ4 = if f_pull then pullCNOTs circ3 shape else circ3

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

-- Used when f_pull flag is active. Moves all CNOTs as much to the beginning as possible
pullCNOTs :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> (qin -> Circ qout)
pullCNOTs circ shape = unencapsulate_generic (x,((arin,theGates',arout,w),ns),y)
  where
    (x,((arin,theGates,arout,w),ns),y) = encapsulate_generic id circ shape
    theGates' = pullCNOTsRec theGates ([],[[] | _ <- [1..w]])

-- The second variable, 'past', holds the circuit up to the point that has been read, in reversed order. It has two components,
--   the first, npast (non-local past) is the earliest part of the circuit, which has already has some CNOTs in it.
--   the second, lpast (local past) is the later parts comprised by 1-qubit gates, and maintained in a different list per wire.
pullCNOTsRec :: [Gate] -> ([Gate],[[Gate]]) -> [Gate]
pullCNOTsRec []     (npast,lpast) = reverse $ (concat lpast) ++ npast
pullCNOTsRec (g:gs) (npast,lpast) = case g of
  (QGate "not" _ [target] [] [signedCtrl] _) -> pullCNOTsRec gs $ (npast', updAt ctrl cpast' $ updAt target tpast' $ lpast)
    where
      updAt w content list = take w list ++ content : drop (w+1) list
      ctrl = from_signed signedCtrl
      (npast', tpast', cpast') = pull target ctrl (npast,lpast !! target,lpast !! ctrl)
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
      pull _ _ (np, [], []) = (g:np, [], [])   
  _ -> pullCNOTsRec gs (npast, updLPastAt $ targetOf g) -- If it's a 1-qubit gate, simply add it to the past  
    where
      updLPastAt w = take w lpast ++ (g:(lpast !! w)) : drop (w+1) lpast



-- ## Building the hypergraph ## --
-- For each wire, a list of the hyperedges it 'controls'. Each (n,ws,m) is a hypedge, ws are the other vertices, and (n,m) is the interval of indexes in [Gate] where it is located
type Hypergraph = M.Map Wire [(Int,[Wire],Int)] 

buildHyp :: [Gate] -> Mode -> Hypergraph
buildHyp gs (f_pull, f_ends, _) = M.map (\wss -> filter (\(_,ws,_) -> not $ null ws) wss) $ buildHypRec gs 0 f_ends

buildHypRec :: [Gate] -> Int -> Bool -> Hypergraph
buildHypRec []     _ _      = M.empty
buildHypRec (g:gs) n f_ends = case g of
  (QGate "not" _ [target] [] [ctrl] _) -> M.alter (addVertex target) (getConnection ctrl) (newHEdgeAt target)
  _ -> newHEdgeAt $ targetOf g
  where
    newHEdgeAt target  = M.alter newHEdge target (buildHypRec gs (n+1) f_ends) -- Subsequent controls on 'target' create a new hyperedge (happens when 'target' is affected by H or CNOT)
    newHEdge tV = case tV of Nothing -> Just [(0,[],n)]
                         --    Just [] -> Just ((0,[],n):[]) 
                             Just ((_,ws,i):es) -> Just ((0,[],n):(n+1,ws,i):es)
    addVertex target cV = case cV of Nothing -> Just [(0,[target],n+1)]
                                     Just ((_,ws,i):es) -> Just ((0,target:ws,i):es)
    getConnection (Signed w s) = if s then w else error $ "DistribHPartError: Negative control" -- As Clifford+T only allows positive controls


-- ## Building the distributed circuit ## --
type Block = Int
type Partition = [Block] -- (partition, eDic)- The wire 'w' is in the block given by: b := (partition !! w). 
type EDic = M.Map (Wire,Block) Wire -- ctrlE := eDic ! (ctrl,btarget). targetE := ctrlE-1. Both are negative integers
data EbitComponent = Entangler (Wire,Block) Int | Disentangler (Wire,Block) Int deriving Show -- (Dis)Entangler (ctrl,btarget) pos

isEntangler :: EbitComponent -> Bool
isEntangler (Entangler    _ _) = True
isEntangler (Disentangler _ _) = False

pos :: EbitComponent -> Int
pos (Entangler    _ n) = n
pos (Disentangler _ n) = n

getConnections :: EbitComponent -> (Wire,Block)
getConnections (Entangler    ws _) = ws
getConnections (Disentangler ws _) = ws

buildCircuit :: (QCData qin, QCData qout) => Partition -> Hypergraph -> (qin, BCircuit, qout) -> Mode -> (qin -> Circ qout)
buildCircuit partition hgraph oldCirc mode = unencapsulate_generic newCirc
  where
    (inp,((ar1,gates,ar2,n),namespace),out) = oldCirc
    -- (eDic,gates',nE) = allocateEbits gates partition hgraph -- REMOVE (it's here for debugging)
    (gates',nE) = distribute gates partition hgraph
    -- reorderWire = \w -> map snd (sortBy (\x y -> compare (fst x) (fst y)) $ zip partition [1..length partition]) !! w
    newCirc = (inp,((ar1,gates',ar2,n+nE),namespace),out)

distribute :: [Gate] -> Partition -> Hypergraph -> ([Gate],Int)
distribute gates partition hypergraph = (distributeCNOTs gates' partition eDic, nE)
  where 
    (eDic,gates', nE) = allocateEbits gates partition hypergraph

allocateEbits :: [Gate] -> Partition -> Hypergraph -> (EDic, [Gate], Int)
allocateEbits gates partition hypergraph = (eDic, allocateEbitsRec components gates partition eDic, nE) 
  where
    components = ebitInfo partition hypergraph
    (nE,eDic) = foldr addToDic (0,M.empty) $ filter isEntangler components
    addToDic (Entangler (ctrl,btarget) _) (w,dic) = (w+2,M.alter (f w) (ctrl, btarget) dic)
    f w x = case x of Nothing -> Just (-w-1); (Just v) -> Just v -- Note: We add to the dictionary only if wires can not be reused

-- Note: The order in which the components are added is essential. It must be from the end to the beginning.
allocateEbitsRec :: [EbitComponent] -> [Gate] -> Partition -> EDic -> [Gate]
allocateEbitsRec []     gates partition eDic = gates
allocateEbitsRec (c:cs) gates partition eDic = insert (allocateEbitsRec cs gates partition eDic)
  where
    ((ctrl,btarget), n, b) = (getConnections c, pos c, isEntangler c)
    ctrlE = eDic M.! (ctrl, btarget) -- eBit control wire
    targetE = ctrlE-1
    insert gs = take n gs ++ component ++ drop n gs
    component = if b 
      then [QInit False targetE False, QInit False ctrlE False, QGate "H" False [ctrlE] [] [] False, QGate "not" False [targetE] [] [Signed ctrlE True] False, QGate "not" False [ctrlE] [] [Signed ctrl True] False, QMeas ctrlE, QGate "not" False [targetE] [] [Signed ctrlE True] False, CDiscard ctrlE]
      else [QGate "H" False [targetE] [] [] False, QMeas targetE, QGate "H" False [ctrl] [] [] False, QGate "not" False [ctrl] [] [Signed targetE True] False, QGate "H" False [ctrl] [] [] False, CDiscard targetE]

-- Produces an ordered list of the components to realise the required ebits (cat-ent/disentanglers). The order is given by ascending position in the circuit.
ebitInfo :: Partition -> Hypergraph -> [EbitComponent]
ebitInfo partition hypergraph = sortBy (\x y -> compare (pos x) (pos y)) $ disentanglers ++ entanglers
  where
    entanglers    = map (\(n,c,b,_) -> Entangler   (c,b) n) eInfo
    disentanglers = map (\(_,c,b,n) -> Disentangler (c,b) n) eInfo
    eInfo    = (filter external . rmdups) $ map (\(i,c,v,o) -> (i,c,partition !! v,o)) flatInfo -- Note: remove internal edges amd duplicates (i.e. CZ edges that can be implemented by the same eBit)
    flatInfo = (concat . concat) $ map (\(c,vss) -> map (\(i,vs,o) -> map (\v -> (i,c,v,o)) vs) vss) (M.toList hypergraph)
    external (_,c,b,_) = partition !! c /= b

distributeCNOTs :: [Gate] -> Partition -> EDic -> [Gate]
distributeCNOTs []     _         _    = []
distributeCNOTs (g:gs) partition eDic = g' : (distributeCNOTs gs partition eDic)
  where
    g' = case g of 
      (QGate "not" rev [target] [] [signedCtrl] ncf) -> let 
        ctrl = from_signed signedCtrl
        fromEbit = ctrl < 0 || target < 0 -- If either of the wires is negative, the CNOT connects to an auxiliar ebit
        bc = partition !! ctrl
        bt = partition !! target
        ctrlE = eDic M.! (ctrl, bt)
        targetE = ctrlE - 1 in
          if fromEbit || bt == bc then g else -- Test if the CNOT is from an ebit or internal of a block; otherwise it must be distributed
            QGate "not" rev [target] [] [Signed targetE True] ncf
      _ -> g

{-
separateWires :: (QCData qin, QCData qout) => (qin -> Circ qout) -> Partition -> (qin -> Circ qout)
separateWires circ partition = \input -> circ $ qcdata_of_endpoints input (organise $ endpoints_of_qcdata input)
  where
    organise ends = if length ends /= length partition then error $ show ends -- "DistribHPartError: Different number of inputs than wires partitioned."
      else map snd (sortBy (\x y -> compare (fst x) (fst y)) $ zip partition ends)
-}

-- ## Parameters ## --
k = "2"
epsilon = "0.03"

-- main = main_circuit ASCII Binary (createOracle 3 5 4)
-- main = print_generic Preview (decompose_generic Toffoli qft_rev) [qubit,qubit,qubit,qubit]
main = let 
  mode  = (True,False,False) -- (PullCNOTs,BothEnds,JoinEbits)
  input = qft_rev
  shape = [qubit,qubit,qubit]
  circ  = prepareCircuit input shape mode
  extractedCirc@(_, ((_,theGates,_,nVertices),_), _) = encapsulate_generic id circ shape
  hypergraph = buildHyp theGates mode
  flatData = concat $ map (\(v,vss) -> map (\(_,vs,_) -> (v+1):(map (+1) vs)) vss) (M.toList hypergraph)
  fileData = (unlines . (map (unwords . (map show)))) $ [length flatData, nVertices] : (map rmdups flatData)
  in do
    print_generic Preview input shape
    print_generic Preview (prepareCircuit input shape (False,False,False)) shape
    print_generic Preview circ shape
    if length flatData == 0 then do
      writeFile "partition.hgr" $ "The circuit can be simplified to only use 1-qubit gates. Partitioning is irrelevant."
      print_generic Preview circ shape
    else do
      writeFile "hypergraph.hgr" $ init fileData -- init to remove last \n
      HSH.run $ "./KaHyPar -h hypergraph.hgr -k "++k++" -e "++epsilon++" -m direct -o km1 -p kahypar/config/km1_direct_kway_alenex17.ini -q true" :: IO ()
      HSH.run $ "mv hypergraph.hgr.part* partition.hgr" :: IO ()
      hypPart <- readFile "partition.hgr"
      let 
        partition = map read (lines hypPart)
        newCircuit = buildCircuit partition hypergraph extractedCirc mode
        in do
          print_generic Preview newCircuit shape
