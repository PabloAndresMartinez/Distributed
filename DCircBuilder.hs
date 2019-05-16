module Distributer.DCircBuilder where

import qualified Data.Map as M
import Data.List (nub, sort, sortBy)

import Quipper
import Quipper.Circuit
import Quipper.Generic

import Distributer.Common

type NonLocalCNOT = (Int,Wire,Wire,Int,Int,HedgeType) -- (i,v,w,p,o,ht); i (o) initial (final) pos of hedge it belongs to, v (w) a wire of the CNOT, p its position, ht its type
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

wasBound :: Wire -> [Gate] -> Bool
wasBound w nextGates = case nextUsed of
    []           -> False -- Not used anymore, so it has already been discarded.
    (ins,outs):_ -> (w,Qbit) `elem` ins -- If it is used, the first time it appears it should be an input, otherwise it's being initalized.
  where
    nextUsed = filter (\(ins,outs) -> (w,Qbit) `elem` ins || (w,Qbit) `elem` outs) $ map gate_arity nextGates

-- ## Circuit bulding routine ## --

buildCircuit :: [Gate] -> Int -> [(Hypergraph, Partition, Int)] -> ([Gate],Int,Int)
buildCircuit oldCirc nWires segments = case segments of
    _:[] -> (distGates, thisWires, thisEbits) 
    _:_  -> (distGates ++ teleGates ++ nextGates, max thisWires nextWires, thisEbits + length teleGates + nextEbits)
  where
    (nextGates, nextWires, nextEbits) = buildCircuit remainingCirc nWires (tail segments)
    (distGates, thisWires, thisEbits) = distribute thisGates thisHyp thisPart 
    (thisGates, remainingCirc) = splitAt thisPos oldCirc
    (thisHyp, thisPart, thisPos) = head segments
    (_,       nextPart, _      ) = head $ tail segments
    teleGates = map (\(w,_) -> teleAt w) $ filter (\(w,b) -> wasBound w remainingCirc && b /= nextPart M.! w) $ M.toList $ M.take nWires thisPart -- Ignore CNOT-vertices!
    teleAt w = QGate "teleport" False [w] [] [] False

distribute :: [Gate] -> Hypergraph -> Partition -> ([Gate],Int,Int)
distribute gates hypergraph partition = (allocateEbits components gatesWithCNOTs eDic 0, nWires, nEbits)
  where 
    gatesWithCNOTs = distributeCNOTs nonlocal gates partition eDic 0
    nonlocal = nonLocalCNOTs partition hypergraph
    components = ebitInfo partition nonlocal
    nEbits = length components `div` 2
    (nWires,eDic) = foldr addToDic (0,M.empty) $ filter isEntangler components
    addToDic (Entangler key _) (w,dic) = if key `M.member` dic then (w,dic) else (w+2, M.insert key (-w-1) dic) -- For each new (c,b,ht) we allocate a new pair of negative wires

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
    component = if htype==Control -- creates the circuit that implements the component c
      then if b 
        then bell ++ [QGate "not" False [sourceE] [] [Signed source True] False, QMeas sourceE, QGate "X" False [sinkE] [] [Signed sourceE True] False, CDiscard sourceE]
        else [QGate "H" False [sinkE] [] [] False, QMeas sinkE, QGate "Z" False [source] [] [Signed sinkE True] False, CDiscard sinkE]
      else if b
        then bell ++ [QGate "not" False [source] [] [Signed sourceE True] False, QGate "H" False [sourceE] [] [] False, QMeas sourceE, QGate "Z" False [sinkE] [] [Signed sourceE True] False, CDiscard sourceE]
        else [QMeas sinkE, QGate "X" False [source] [] [Signed sinkE True] False, CDiscard sinkE]
    bell = [QInit False sinkE False, QInit False sourceE False, QGate "H" False [sourceE] [] [] False, QGate "not" False [sinkE] [] [Signed sourceE True] False]

-- Produces an ordered list of the components to realise the required ebits (cat-ent/disentanglers). The order is given by ascending position in the circuit.
ebitInfo :: Partition -> [NonLocalCNOT] -> [EbitComponent]
ebitInfo partition nonlocal = sort $ disentanglers ++ entanglers
  where
    entanglers    = map (\(n,c,b,_,ht) -> Entangler    (c,b,ht) n) eInfo
    disentanglers = map (\(_,c,b,n,ht) -> Disentangler (c,b,ht) n) eInfo
    eInfo = nub $ map (\(i,c,w,_,o,ht) -> (i,c,partition M.! w,o,ht)) nonlocal -- This nub makes sure there's only one element per cut between gate positions [i,o]

nonLocalCNOTs :: Partition -> Hypergraph -> [NonLocalCNOT]
nonLocalCNOTs partition hyp = sortBy (\(_,_,_,pos1,_,_) (_,_,_,pos2,_,_) -> compare pos1 pos2) nonlocal
  where
    nonlocal = filter (\(_,src,snk,_,_,_) -> partition M.! src /= partition M.! snk) cnots
    cnots    = foldr (\(i,v,ws,o,ht) cs -> map (\(w,p) -> (i,v,w,p,o,ht)) ws ++ cs) [] hedges
    hedges   = M.foldrWithKey (\v vss hs -> map (\(i,ws,o,ht) -> (i,v,ws,o,ht)) vss ++ hs) [] hyp

distributeCNOTs :: [NonLocalCNOT] -> [Gate] -> Partition -> EDic -> Int -> [Gate]
distributeCNOTs []     gs partition _    prev = gs
distributeCNOTs (c:cs) gs partition eDic prev = gsInit ++ distributeCNOTs cs (g':tail gsTail) partition eDic pos
  where
    gsInit  = take (pos-prev) gs
    gsTail  = drop (pos-prev) gs
    g' = case head gsTail of
      (QGate "not" rev [target] [] [Signed ctrl True] ncf) -> if ht==Control 
        then QGate "not" rev [target] [] [Signed ebit True] ncf
        else QGate "not" rev [ebit]   [] [Signed ctrl True] ncf
      _ -> error "Failure when distributing CNOTs"
    (_, source, sink, pos, _, ht) = c 
    ebit = eDic M.! (source, partition M.! sink, ht) - 1
