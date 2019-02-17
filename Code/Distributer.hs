import Quipper
import Quipper.Generic
import Quipper.Monad
import Quipper.QData
import Quipper.Circuit
import QuipperLib.Decompose
import QuipperLib.QFT
import System.Random
import System.Environment
import Libraries.RandomSource
import QuipperLib.Unboxing

import qualified Data.Map as M
import Data.List (sort, sortBy, (!!))
import qualified HSH as HSH
import Distributer.Examples
import qualified Distributer.Configuration as Cfg

type Mode = (Bool,Bool) -- (PullCNOTs,BothRemotes)

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
  where standardError = "DistribHPartError: Gate "++show gate++" was not properly prepocessed."

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
    circ1   = transform_generic swapRemover (unbox_recursive $ decompose_generic TrimControls (decompose_generic cliffordT circ)) -- Decompose to Clifford+T and inline all subroutines *PROVISIONAL* 
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


-- ## Building the hypergraph ## --
-- For each wire, a list of the hyperedges it 'controls'. Each (n,ws,m,b) is a hypedge:
--   ws are the other vertices, pairs (wire,pos), the wire and the position of the CNOT, 
--   (n,m) is the interval of indexes in [Gate] where it is located,
--   b==Control indicates the key is control (· dot), otherwise the key is target (+ dot)
type Hypergraph = M.Map Wire [(Int,[(Wire,Int)],Int,HedgeType)]
data HedgeType = Control | Target | Unknown deriving (Eq, Ord, Show)

buildHyp :: [Gate] -> Int -> Mode -> Hypergraph
buildHyp gs n (_, f_both) = M.map (filter (\(_,ws,_,_) -> not $ null ws)) hyp -- Remove all singleton (unconnected) hyperedges
  where
    hyp = if f_both then toPositive (bothHyp gs 0 0) else vanillaHyp gs 0 -- Build the hypergraph by exploring the gates recursively
    toPositive = M.map (map (\(i,ws,o,ht) -> (i,map (\(w,p) -> (n-w-1,p)) ws,o,ht)))  -- Convert all negative auxiliary wires to positive ones, so KaHyPart does not explode

vanillaHyp :: [Gate] -> Int -> Hypergraph
vanillaHyp []     _ = M.empty
vanillaHyp (g:gs) n = let
 newHEdgeAt wire = M.alter newHEdge wire $ vanillaHyp gs (n+1) -- Subsequent CNOTs on 'wire' create a new hyperedge
 newHEdge v = case v of Nothing -> Just [(nan,[],n,Control)]
                        Just ((_,ws,o,ht):es) -> Just ((nan,[],n,Control):(n+1,ws,o,ht):es)
 nan = 0
 in case g of
  (QGate "not" _ [target] [] [signedCtrl] _) -> newCNOTAt control target
    where
      newCNOTAt ctrl target = M.alter (newCNOT target) ctrl $ M.alter newHEdge target $ vanillaHyp gs (n+1)
      newCNOT target v = case v of Nothing -> Just [(nan,[(target,n)],n+1,Control)] -- n+1 because it finishes AFTER this gate
                                   Just ((_,ws,o,ht):es) -> Just ((nan,(target,n):ws,o,ht):es) -- Add the cnot to the hyperedge, as they are of the same type
      control = getWire signedCtrl
      getWire (Signed w s) = if s then w else error $ "DistribHPartError: Negative control" -- As Clifford+T only allows positive controls
  _ -> newHEdgeAt $ targetOf g

bothHyp :: [Gate] -> Int -> Int -> Hypergraph
bothHyp []     _ _    = M.empty
bothHyp (g:gs) n cnot = case g of
  (QGate "not" _ [target] [] [signedCtrl] _) -> newCNOTAt control target
    where
      newCNOTAt ctrl target = M.alter (newCNOT Target cnot) target $ M.alter (newCNOT Control cnot) ctrl $ bothHyp gs (n+1) (cnot-1)
      newCNOT hType cnot v = case v of Nothing -> Just [(nan,[(cnot-1,n)],n+1,hType)] -- n+1 because it finishes AFTER this gate
                                       Just ((_,ws,o,ht):es) -> 
                                          if ht==hType then Just ((nan,(cnot-1,n):ws,o,ht):es) -- Add the cnot to the hyperedge, as they are of the same type
                                          else Just ((nan,[(cnot-1,n)],n+1,hType):(n+1,ws,o,ht):es) -- The previous hyperedge was of the other type, so close it and create a new hyperedge
      nan = 0
      control = getWire signedCtrl
      getWire (Signed w s) = if s then w else error $ "DistribHPartError: Negative control" -- As Clifford+T only allows positive controls
  _ -> newHEdgeAt $ targetOf g
    where
      newHEdgeAt wire  = M.alter newHEdge wire $ bothHyp gs (n+1) cnot -- Subsequent CNOTs on 'wire' create a new hyperedge
      newHEdge v = case v of Nothing -> Just [(nan,[],n,Unknown)]
                             Just ((_,ws,o,ht):es) -> Just ((nan,[],n,Unknown):(n+1,ws,o,ht):es)
      nan = 0

hypToString :: Cfg.PartAlg -> Hypergraph -> Int -> (String, Int, Int)
hypToString alg hyp n = (fileData, nHedges, nVertices)
  where    
    flatData = concat $ map (\(v,vss) -> map (\(_,vs,_,_) -> (v+1):(map (\(w,_) -> w+1) vs)) vss) (M.toList hyp)
    fileData = (unlines . (map (unwords . (map show)))) $ (fstLine alg) : (map rmdups flatData) ++ verticesWeights
    fstLine Cfg.Kahypar = [nHedges, nVertices, 10]
    fstLine Cfg.Patoh   = [1, nVertices, nHedges, (nVertices-n)*2+nHedges, 1]
    verticesWeights = [[1] | _ <- [1..n]] ++ [[0] | _ <- [(n+1)..nVertices]]
    nVertices = foldr (max . foldr max 0) 0 flatData
    nHedges   = length flatData

-- ## Building the distributed circuit ## --
type Block = Int
type Partition = [Block] -- (partition, eDic)- The wire 'w' is in the block given by: b := (partition !! w). 
type EDic = M.Map (Wire,Block,HedgeType) Wire -- sourceE := eDic ! (ctrl,btarget). sinkE := sourceE-1. Both are negative integers
data EbitComponent = Entangler (Wire,Block,HedgeType) Int | Disentangler (Wire,Block,HedgeType) Int deriving (Show,Eq) -- (Dis)Entangler (ctrl,btarget,hType) pos

instance Ord EbitComponent where
  compare c1 c2 = case compare (pos c1) (pos c2) of
    LT -> LT; GT -> GT
    EQ -> case compare (isEntangler c1) (isEntangler c2) of
      LT -> LT; GT -> GT
      EQ -> compare (getConnections c1) (getConnections c2)

isEntangler :: EbitComponent -> Bool
isEntangler (Entangler    _ _) = True
isEntangler (Disentangler _ _) = False

pos :: EbitComponent -> Int
pos (Entangler    _ n) = n
pos (Disentangler _ n) = n

getConnections :: EbitComponent -> (Wire,Block,HedgeType)
getConnections (Entangler    ws _) = ws
getConnections (Disentangler ws _) = ws

buildCircuit :: (QCData qin, QCData qout) => Partition -> Hypergraph -> (qin, BCircuit, qout) -> (qin -> Circ qout, Int, Int)
buildCircuit partition hgraph oldCirc = (unencapsulate_generic newCirc, nWires, nEbits)
  where
    (inp,((ar1,gates,ar2,n),namespace),out) = oldCirc
    (gates',nWires,nEbits) = distribute gates partition hgraph
    newCirc = (inp,((ar1,gates',ar2,n+nWires),namespace),out)

distribute :: [Gate] -> Partition -> Hypergraph -> ([Gate],Int,Int)
distribute gates partition hypergraph = (allocateEbits components gatesWithCNOTs eDic 0, nWires, nEbits)
  where 
    gatesWithCNOTs = distributeCNOTs cnots gates partition eDic 0
    cnots = nonLocalCNOTs gates partition hypergraph
    components = ebitInfo partition hypergraph
    nEbits = length components `div` 2
    (nWires,eDic) = foldr addToDic (0,M.empty) $ filter isEntangler components
    addToDic (Entangler key _) (w,dic) = if key `M.member` dic then (w,dic) else (w+2, M.insert key (-w-1) dic) -- We add to the dictionary only if wires can not be reused

-- Note: The order how the components are added is essential. It must be from the end to the beginning.
allocateEbits :: [EbitComponent] -> [Gate] -> EDic -> Int -> [Gate]
allocateEbits []     gates eDic _    = gates
allocateEbits (c:cs) gates eDic prev = gatesInit ++ component ++ allocateEbits cs gatesTail eDic n
  where
    gatesInit = take (n-prev) gates
    gatesTail = drop (n-prev) gates
    ((source,bsink,htype), n, b) = (getConnections c, pos c, isEntangler c)
    sourceE = eDic M.! (source, bsink, htype) -- eBit wire for hyperedge 'source' (i.e. control if control type, target otherwise)
    sinkE = sourceE-1
    component = if htype==Control 
      then if b 
        then bell ++ [QGate "not" False [sourceE] [] [Signed source True] False, QMeas sourceE, QGate "X" False [sinkE] [] [Signed sourceE True] False, CDiscard sourceE]
        else [QGate "H" False [sinkE] [] [] False, QMeas sinkE, QGate "Z" False [source] [] [Signed sinkE True] False, CDiscard sinkE]
      else if b
        then bell ++ [QGate "not" False [source] [] [Signed sourceE True] False, QGate "H" False [sourceE] [] [] False, QMeas sourceE, QGate "Z" False [sinkE] [] [Signed sourceE True] False, CDiscard sourceE]
        else [QMeas sinkE, QGate "X" False [source] [] [Signed sinkE True] False, CDiscard sinkE]
    bell = [QInit False sinkE False, QInit False sourceE False, QGate "H" False [sourceE] [] [] False, QGate "not" False [sinkE] [] [Signed sourceE True] False]

-- Produces an ordered list of the components to realise the required ebits (cat-ent/disentanglers). The order is given by ascending position in the circuit.
ebitInfo :: Partition -> Hypergraph -> [EbitComponent]
ebitInfo partition hypergraph = sort $ disentanglers ++ entanglers
  where
    entanglers    = map (\(n,c,b,_,ht) -> Entangler    (c,b,ht) n) eInfo
    disentanglers = map (\(_,c,b,n,ht) -> Disentangler (c,b,ht) n) eInfo
    eInfo    = (filter external . rmdups) $ map (\(i,c,(w,p),o,ht) -> (i,c,partition !! w,o,ht)) flatInfo -- Note: remove internal edges and duplicates (i.e. CNOT edges that can be implemented by the same eBit)
    flatInfo = (concat . concat) $ map (\(c,vss) -> map (\(i,vs,o,ht) -> map (\v -> (i,c,v,o,ht)) vs) vss) (M.toList hypergraph)
    external (_,c,b,_,_) = partition !! c /= b

nonLocalCNOTs :: [Gate] -> Partition -> Hypergraph -> [(Wire,Wire,Int,HedgeType)]
nonLocalCNOTs gs partition hyp = sortBy (\(_,_,pos1,_) (_,_,pos2,_) -> compare pos1 pos2) nonlocal
  where
    hedges = concat $ map (\(v,vss) -> map (\(_,ws,_,ht) -> (v,ws,ht)) vss) (M.toList hyp)
    cnots  = concat $ map (\(v,ws,ht) -> map (\(w,p) -> (v,w,p,ht)) ws) hedges 
    nonlocal = filter (\(src,snk,_,_) -> partition !! src /= partition !! snk) cnots

distributeCNOTs :: [(Wire,Wire,Int,HedgeType)] -> [Gate] -> Partition -> EDic -> Int -> [Gate]
distributeCNOTs []     gs partition _    prev = gs
distributeCNOTs (c:cs) gs partition eDic prev = gsInit ++ distributeCNOTs cs gsTail partition eDic pos
  where
    gsInit = take (pos-prev) gs
    gsTail = g' : drop (pos-prev+1) gs
    g' = case gs !! (pos-prev) of
      (QGate "not" rev [target] [] [Signed ctrl True] ncf) -> if ht==Control 
        then QGate "not" rev [target] [] [Signed ebit True] ncf
        else QGate "not" rev [ebit]   [] [Signed ctrl True] ncf
      _ -> error "DistribHPartError: Failure when distributing CNOTs"
    (source, sink, pos, ht) = c 
    ebit = eDic M.! (source, partition !! sink, ht) - 1
    
-- ## Building the distributed circuit ## --
main = do
  (input,shape, name) <- Cfg.circuit
  let
    k = Cfg.k; epsilon = Cfg.epsilon; mode = (Cfg.pullCNOTs, Cfg.bothRemotes)
    circ  = prepareCircuit input shape mode
    extractedCirc@(_, ((_,theGates,_,nWires),_), _) = encapsulate_generic id circ shape
    hypergraph = buildHyp theGates nWires mode
    (fileData, hypHEdges, hypVertices) = hypToString Cfg.algorithm hypergraph nWires
    script Cfg.Kahypar = Cfg.partDir++"KaHyPar -h hypergraph.hgr -k "++k++" -e "++epsilon++" -m direct -o km1 -p "++Cfg.partDir++Cfg.subalgorithm++" -q true"
    script Cfg.Patoh = Cfg.partDir++"PaToH hypergraph.hgr "++k++" FI="++epsilon++" UM=O PQ=Q OD=0 PA=13 RA=0"
    in
      if head fileData == '0' then do
        print_generic Cfg.outputAs circ shape
        putStrLn $ "The circuit can be simplified to only use 1-qubit gates. Partitioning is irrelevant."
      else do
        writeFile "hypergraph.hgr" $ fileData
        HSH.run $ script Cfg.algorithm :: IO ()
        HSH.run $ "mv hypergraph.hgr.part* partition.hgr" :: IO ()
        hypPart <- readFile "partition.hgr"
        let 
          gateCountInput = length theGates
          partition = map read (concat . map words . lines $ hypPart)
          (newCircuit, nExtraWires, nEbits) = buildCircuit partition hypergraph extractedCirc
          in do
            --print_generic Preview input shape
            --print_generic Preview (prepareCircuit input shape (False,False,False)) shape
            --print_generic Preview circ shape
            --print_generic Preview newCircuit shape
            putStrLn $ ""
            putStrLn $ "Gate count:"
            print_generic Cfg.outputAs newCircuit shape
            putStrLn $ "Original gate count: " ++ show gateCountInput
            putStrLn $ "Original qubit count: " ++ show nWires
            putStrLn $ "Partition: " ++ show (take nWires partition)
            putStrLn $ "Total number of ebits: " ++ show nEbits
            putStrLn $ ""
            putStrLn $ "Number of vertices: " ++ show hypVertices
            putStrLn $ "Number of hyperedges: " ++ show hypHEdges
            putStrLn $ ""
            putStrLn $ "Circuit: "++name
            putStrLn $ "Extensions: " ++ (if fst mode then "PullCNOTs (limit: "++show Cfg.pullLimit++"), " else "") ++ (if snd mode then "BothRemotes, " else "")
            putStrLn $ "k = "++show k++"; epsilon = "++show epsilon