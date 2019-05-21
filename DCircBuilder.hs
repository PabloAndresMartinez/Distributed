module Distributer.DCircBuilder where

import qualified Data.Map as M
import Data.List (nub, sort, sortBy)

import Quipper
import Quipper.Circuit
import Quipper.Generic

import Distributer.Common

type NonLocalCZ = (Int,Wire,Wire,Int,Int) -- (i,v,w,p,o); i (o) initial (final) pos of hedge it belongs to, v and w are the wires the CZ acts on, p its position
type EDic = M.Map (Wire,Block) Wire -- sourceE := eDic ! (ctrl,btarget). sinkE := sourceE-1. Both are negative integers
data EbitComponent = Entangler (Wire,Block) Int | Disentangler (Wire,Block) Int deriving (Show,Eq) -- (Dis)Entangler (ctrl,btarget,hType) pos
type BindingFlags = M.Map Wire Bool

instance Ord EbitComponent where
  compare c1 c2 = case compare (position c1) (position c2) of
    LT -> LT; GT -> GT
    EQ -> case compare (isEntangler c1) (isEntangler c2) of
      LT -> LT; GT -> GT
      EQ -> compare (getConnections c1) (getConnections c2)

isEntangler :: EbitComponent -> Bool
isEntangler (Entangler    _ _) = True
isEntangler (Disentangler _ _) = False

position :: EbitComponent -> Int
position (Entangler    _ n) = n
position (Disentangler _ n) = n

getConnections :: EbitComponent -> (Wire,Block)
getConnections (Entangler    ws _) = ws
getConnections (Disentangler ws _) = ws

updBindings :: [Gate] -> BindingFlags -> BindingFlags
updBindings []     bindings = bindings
updBindings (g:gs) bindings = updBindings gs bindings''
  where
    bindings'' = foldr (\w bs -> M.insert w True bs)  bindings' $ filterQbit outs
    bindings'  = foldr (\w bs -> M.insert w False bs) bindings  $ filterQbit ins
    (ins,outs) = gate_arity g
    filterQbit arity = map fst $ filter (\(w,t) -> case t of Qbit -> True; _ -> False) arity

-- ## Circuit bulding routine ## --

buildCircuit :: [Gate] -> Int -> Int -> [(Hypergraph, Partition, Int)] -> ([Gate],Int,Int,Int)
buildCircuit oldCirc nWires boundWires segments = buildCircuitRec oldCirc nWires  initialBindings segments
  where
    initialBindings = M.fromList $ [(w,True) | w <- [0..boundWires-1]] ++ [(w,False) | w <- [boundWires..nWires-1]]

buildCircuitRec :: [Gate] -> Int -> BindingFlags -> [(Hypergraph, Partition, Int)] -> ([Gate],Int,Int,Int)
buildCircuitRec oldCirc nWires bindings segments = case segments of
    _:[] -> (distGates, thisWires, thisEbits, 0) 
    _:_  -> (distGates ++ teleGates ++ nextGates, max thisWires nextWires, thisEbits + nextEbits, length teleGates + nextTPs)
  where
    (nextGates, nextWires, nextEbits, nextTPs) = buildCircuitRec remainingCirc nWires bindings' (tail segments) 
    (distGates, thisWires, thisEbits) = distribute thisGates thisHyp thisPart 
    (thisGates, remainingCirc) = splitAt thisPos oldCirc
    (thisHyp, thisPart, thisPos) = head segments
    (_,       nextPart, _      ) = head $ tail segments
    bindings' = updBindings thisGates bindings
    teleGates = map (\(w,_) -> teleAt w) $ filter (\(w,b) -> bindings' M.! w && b /= nextPart M.! w) $ M.toList $ M.take nWires thisPart -- Ignore CZ-vertices!
    teleAt w = QGate "teleport" False [w] [] [] False

distribute :: [Gate] -> Hypergraph -> Partition -> ([Gate],Int,Int)
distribute gates hypergraph partition = (allocateEbits components gatesWithCZs eDic 0, nWires, nEbits)
  where 
    gatesWithCZs = distributeCZs nonlocal gates partition eDic 0
    nonlocal = nonLocalCZs partition hypergraph
    components = ebitInfo partition nonlocal
    nEbits = length components `div` 2
    (nWires,eDic) = foldr addToDic (0,M.empty) $ filter isEntangler components
    addToDic (Entangler key _) (w,dic) = if key `M.member` dic then (w,dic) else (w+2, M.insert key (-w-1) dic) -- For each new (c,b) we allocate a new pair of negative wires

-- Note: The order how the components are added is essential. It must be from the end to the beginning.
allocateEbits :: [EbitComponent] -> [Gate] -> EDic -> Int -> [Gate]
allocateEbits []     gates eDic _    = gates
allocateEbits (c:cs) gates eDic prev = gatesInit ++ component ++ allocateEbits cs gatesTail eDic n
  where
    gatesInit = take (n-prev) gates
    gatesTail = drop (n-prev) gates
    ((source,bsink), n) = (getConnections c, position c)
    sourceE = eDic M.! (source, bsink) -- eBit wire for hyperedge 'source' (i.e. control if control type, target otherwise)
    sinkE = sourceE-1
    component = if isEntangler c
      then bell ++ [QGate "not" False [sourceE] [] [Signed source True] False, QMeas sourceE, QGate "X" False [sinkE] [] [Signed sourceE True] False, CDiscard sourceE]
      else [QGate "H" False [sinkE] [] [] False, QMeas sinkE, QGate "Z" False [source] [] [Signed sinkE True] False, CDiscard sinkE]
    bell = [QInit False sinkE False, QInit False sourceE False, QGate "H" False [sourceE] [] [] False, QGate "not" False [sinkE] [] [Signed sourceE True] False]

-- Produces an ordered list of the components to realise the required ebits (cat-ent/disentanglers). The order is given by ascending position in the circuit.
ebitInfo :: Partition -> [NonLocalCZ] -> [EbitComponent]
ebitInfo partition nonlocal = sort $ disentanglers ++ entanglers
  where
    entanglers    = map (\(n,c,b,_) -> Entangler    (c,b) n) eInfo
    disentanglers = map (\(_,c,b,n) -> Disentangler (c,b) n) eInfo
    eInfo = nub $ map (\(i,c,w,_,o) -> (i,c,partition M.! w,o)) nonlocal -- This nub makes sure there's only one element per cut between gate positions [i,o]

nonLocalCZs :: Partition -> Hypergraph -> [NonLocalCZ]
nonLocalCZs partition hyp = sortBy (\(_,_,_,pos1,_) (_,_,_,pos2,_) -> compare pos1 pos2) nonlocal
  where
    nonlocal = filter (\(_,src,snk,_,_) -> partition M.! src /= partition M.! snk) czs
    czs      = foldr (\(i,v,ws,o) cs -> map (\(w,p) -> (i,v,w,p,o)) ws ++ cs) [] hedges
    hedges   = M.foldrWithKey (\v vss hs -> map (\(i,ws,o) -> (i,v,ws,o)) vss ++ hs) [] hyp

distributeCZs :: [NonLocalCZ] -> [Gate] -> Partition -> EDic -> Int -> [Gate]
distributeCZs []     gs partition _    prev = gs
distributeCZs (c:cs) gs partition eDic prev = gsInit ++ distributeCZs cs (g':tail gsTail) partition eDic pos
  where
    gsInit  = take (pos-prev) gs
    gsTail  = drop (pos-prev) gs
    g' = QGate "CZ" False [target] [] [Signed ebit True] False
    (_, source, sink, pos, _) = c 
    ebit = eDic M.! (source, partition M.! sink) - 1
    (w1,w2) = getWires (head gsTail)
    target = if w1 == source then w2 else w1
