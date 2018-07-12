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


-- ## Extended Quipper fucntions ## --
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
prepareCircuit :: (QCData qin, QCData qout) => Mode -> (qin -> Circ qout) -> (qin -> Circ qout)
prepareCircuit (f_pull, _, _) circ = if f_pull then pullCNOTs circ''' else circ'''
  where
    circ'   = unbox_recursive (transform_generic swapRemover $ decompose_generic TrimControls (decompose_generic cliffordT circ)) -- Decompose to Clifford+T and inline all subroutines *PROVISIONAL* 
    circ''  = \inp -> without_comments $ circ' inp -- Ignore comments
    circ''' = separateClassicalControl circ''

-- Moves
pullCNOTs :: (QCData qin, QCData qout) => (qin -> Circ qout) -> (qin -> Circ qout)
pullCNOTs = id

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
                if quantumCtrls then qnot_at target `controlled` ctrls else gate_X_at target `controlled` ctrls
                return ([target], [], ctrls) 
      | otherwise = identity_transformer g
    separator g   = identity_transformer g

{-
-- prepareCircuit has to be called on 'circ' before giving it as argument to 'extractGates'
extractGates :: (QCData qin, QCData qout) => (qin -> Circ qout) -> qin -> ([Gate],Int)
extractGates circ shape = (theGates, nVertices) 
  where
    (_,theGates,_,nVertices) = (fst . (\(_,c,_) -> c)) $ encapsulate_generic errMsg circ shape -- Ignore I/O shape, Namespace (i.e. subroutine info) and (inArity, _ , outArity, nWires)
    errMsg = id -- Do no filtering/formatting of error messages
-}

-- ## Building the hypergraph ## --
-- For each wire, a list of the hyperedges it 'controls'. Each (n,ws,m) is a hypedge, ws are the other vertices, and (n,m) is the interval of indexes in [Gate] where it is located
type Hypergraph = M.Map Wire [(Int,[Wire],Int)] 


buildHyp :: [Gate] -> Mode -> Hypergraph
buildHyp gs (_, f_ends, _) = M.map (\wss -> filter (\(_,ws,_) -> not $ null ws) wss) $ buildHypRec gs 0 f_ends

buildHypRec :: [Gate] -> Int -> Bool -> Hypergraph
buildHypRec []     _ _      = M.empty
buildHypRec (g:gs) n f_ends = case g of
  (QGate "not" _ [target] [] []    _) -> error standardError         -- A 'not' with no controls is just an X gate, but it should never appear like this
  (QGate "not" _ [target] [] ctrls _) -> foldr (\c h -> M.alter (addVertex target) c h) (hypAddHEdge target) (getConnections ctrls)
  (QGate "X"   _ [target] [] _     _) -> hypAddHEdge target          -- Prevents next controls on 'target' from joining the previous hyperedge
  (QGate "Y"   _ [target] [] []    _) -> hypAddHEdge target          -- Prevents next controls on 'target' from joining the previous hyperedge
  (QGate "Z"   _ _        [] []    _) -> buildHypRec gs (n+1) f_ends -- Skip, can be pushed through control trivially
  (QGate "S"   _ _        [] []    _) -> buildHypRec gs (n+1) f_ends -- Skip, can be pushed through control trivially
  (QGate "T"   _ _        [] []    _) -> buildHypRec gs (n+1) f_ends -- Skip, can be pushed through control trivially
  (QGate "H"   _ [target] [] []    _) -> hypAddHEdge target          -- Prevents next controls on 'target' from joining the previous hyperedge *PROVISIONAL* (can be optimised to cancel with target of a CNOT!)
  (QGate _     _ _        _  _     _) -> error standardError
  (QPrep _ _)                         -> buildHypRec gs (n+1) f_ends -- Skip, no effect on the hypergraph
  (QUnprep target _)                  -> hypAddHEdge target          -- Qubit termination prevents hyperedge joining
  (QInit _ _ _)                       -> buildHypRec gs (n+1) f_ends -- Skip, no effect on the hypergraph
  (CInit _ _ _)                       -> buildHypRec gs (n+1) f_ends -- Skip, no effect on the hypergraph
  (QTerm _ target _)                  -> hypAddHEdge target          -- Qubit termination prevents hyperedge joining
  (CTerm _ target _)                  -> hypAddHEdge target          -- Qubit termination prevents hyperedge joining
  (QMeas target)                      -> hypAddHEdge target          -- Qubit termination prevents hyperedge joining
  (QDiscard target)                   -> hypAddHEdge target          -- Qubit termination prevents hyperedge joining
  (CDiscard target)                   -> hypAddHEdge target          -- Qubit termination prevents hyperedge joining
  _ -> error standardError
  where
    hypAddHEdge target  = M.alter newHEdge target (buildHypRec gs (n+1) f_ends) -- Subsequent controls on 'target' create a new hyperedge (happens when 'target' is affected by H or CNOT)
    newHEdge tV = case tV of Nothing -> Just [(0,[],n)]
                         --    Just [] -> Just ((0,[],n):[]) 
                             Just ((_,ws,i):es) -> Just ((0,[],n):(n+1,ws,i):es)
    addVertex target cV = case cV of Nothing -> Just [(0,[target],n+1)]
                                     Just ((_,ws,i):es) -> Just ((0,target:ws,i):es)
    getConnections ctrls = map (\(Signed w s) -> if s then w else error $ "HypPartError: Negative control") ctrls -- Assuming Clifford+T only allows positive controls
    standardError = "HypPartError: "++show g++" is not handled. Preprocessing should remove it." 

{-
data WireType = AQubit | ABit | Discarded 
type WireDic  = M.Map Wire WireType

-- Ignore classical controls and push Paulis if indicated by the flag
refactor :: (QCData qin) => [Gate] -> qin -> Bool -> [Gate]
refactor gs shape = refactorRec gs shapeDic
  where
    refactorRec []     _     _       = []
    refactorRec (g:gs) dic f_pull = case g of
      (QGate "not" _ [target] [] []    _) -> error standardError  -- A 'not' with no controls is just an X gate, but it should never appear like this
      (QGate "not" _ [target] [] ctrls _) -> undefined -- Test if controls are classical
      (QGate "X"   _ [target] [] []    _) -> 
      (QGate "Y"   _ [target] [] []    _) -> 
      (QGate "Z"   _ _        [] []    _) -> 
      (QGate "S"   _ _        [] []    _) -> 
      (QGate "T"   _ _        [] []    _) -> 
      (QGate "H"   _ [target] [] []    _) -> 
      (QGate _     _ _        _  _     _) -> error standardError
      (QPrep _ _)                         -> 
      (QUnprep target _)                  -> 
      (QInit _ _ _)                       -> 
      (CInit _ _ _)                       -> 
      (QTerm _ target _)                  -> g : (refactorRec gs (insert target Discarded dic) f_pull)
      (CTerm _ target _)                  -> g : (refactorRec gs (insert target Discarded dic) f_pull)
      (QMeas target)                      -> g : (refactorRec gs (insert target ABit dic) f_pull)
      (QDiscard target)                   -> g : (refactorRec gs (insert target Discarded dic) f_pull)
      (CDiscard target)                   -> g : (refactorRec gs (insert target Discarded dic) f_pull)
      _ -> error standardError
    standardError = "HypPartError: "++show g++" is not handled. Preprocessing should remove it." 
    shapeDic = M.fromList $ zip [0..] (map ftype $ qcdata_sequentialize shape shape)
    ftype e = case e of (Endpoint_Qubit _) -> AQubit; (Endpoint_Bit _) -> ABit
-}

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
    organise ends = if length ends /= length partition then error $ show ends -- "HypPartError: Different number of inputs than wires partitioned."
      else map snd (sortBy (\x y -> compare (fst x) (fst y)) $ zip partition ends)
-}

-- ## Parameters ## --
k = "3"
epsilon = "0.03"

-- main = main_circuit ASCII Binary (createOracle 3 5 4)
-- main = print_generic Preview (decompose_generic Toffoli qft_rev) [qubit,qubit,qubit,qubit]
main = let 
  mode  = (False,False,False)
  circ  = prepareCircuit mode classical2
  shape = (bit,bit,qubit,qubit,qubit)
  extractedCirc@(_, ((_,theGates,_,nVertices),_), _) = encapsulate_generic id circ shape
  hypergraph = buildHyp theGates mode
  flatData = concat $ map (\(v,vss) -> map (\(_,vs,_) -> (v+1):(map (+1) vs)) vss) (M.toList hypergraph)
  fileData = (unlines . (map (unwords . (map show)))) $ [length flatData, nVertices] : (map rmdups flatData)
  in do
    print_generic Preview circ shape
    writeFile "hypergraph.hgr" $ init fileData -- init to remove last \n
    HSH.run $ "./KaHyPar -h hypergraph.hgr -k "++k++" -e "++epsilon++" -m direct -o km1 -p kahypar/config/km1_direct_kway_alenex17.ini -q true" :: IO ()
    HSH.run $ "mv hypergraph.hgr.part* partition.hgr" :: IO ()
    hypPart <- readFile "partition.hgr"
    let 
      partition = map read (lines hypPart)
      newCircuit = buildCircuit partition hypergraph extractedCirc mode
      in do
        print_generic Preview newCircuit shape
