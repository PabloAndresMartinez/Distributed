module Distributer.DCircBuilder where

import qualified Data.Map as M
import Data.List (nub, sort, sortBy, (\\))

import Quipper
import Quipper.Internal.Circuit
import Quipper.Internal.Generic

import Distributer.Common

type NonLocalConnection = (Int,Wire,Wire,Int,Int) -- (i,v,w,p,o); i (o) initial (final) pos of hedge it belongs to, v and w are the wires the CZ acts on, p its position
type EDic = M.Map (Wire,Block) Wire -- sourceE := eDic ! (ctrl, bctrl, btarget). sinkE := sourceE-1. Both are negative integers
data EbitComponent = Entangler (Wire,Block) Block Int | Disentangler (Wire,Block) Block Int deriving (Show,Eq) -- (Dis)Entangler (ctrl,btarget) bctrl pos
type BindingFlags = M.Map Wire Bool

instance Ord EbitComponent where
  compare c1 c2 = case compare (position c1) (position c2) of
    LT -> LT; GT -> GT
    EQ -> case compare (isEntangler c1) (isEntangler c2) of
      LT -> LT; GT -> GT
      EQ -> compare (getConnections c1) (getConnections c2)

isEntangler :: EbitComponent -> Bool
isEntangler (Entangler    _ _ _) = True
isEntangler (Disentangler _ _ _) = False

position :: EbitComponent -> Int
position (Entangler    _ _ n) = n
position (Disentangler _ _ n) = n

getConnections :: EbitComponent -> ((Wire,Block),Block)
getConnections (Entangler    ws bs _) = (ws,bs)
getConnections (Disentangler ws bs _) = (ws,bs)

updBindings :: [Gate] -> BindingFlags -> BindingFlags
updBindings []     bindings = bindings
updBindings (g:gs) bindings = updBindings gs bindings''
  where
    bindings'' = foldr (\w bs -> M.insert w True bs)  bindings' $ filterQbit outs
    bindings'  = foldr (\w bs -> M.insert w False bs) bindings  $ filterQbit ins
    (ins,outs) = gate_arity g
    filterQbit arity = map fst $ filter (\(w,t) -> case t of Qbit -> True; _ -> False) arity

-- ## Circuit bulding routine ## --

buildCircuit :: Int -> Int -> [Segment] -> ([Gate],Int,Int,Int)
buildCircuit nWires boundWires segments = buildCircuitRec nWires initialBindings segments
  where
    initialBindings = M.fromList $ [(w,True) | w <- [0..boundWires-1]] ++ [(w,False) | w <- [boundWires..nWires-1]]

buildCircuitRec :: Int -> BindingFlags -> [Segment] -> ([Gate],Int,Int,Int)
buildCircuitRec nWires bindings segments = case segments of
    _:[] -> (distGates', thisWires, thisEbits, 0) 
    _:_  -> (distGates' ++ teleGates ++ nextGates, max thisWires nextWires, thisEbits + nextEbits, length teleGates + nextTPs)
  where
    distGates' = addPartComments bindings thisPart distGates
    (nextGates, nextWires, nextEbits, nextTPs) = buildCircuitRec nWires bindings' (tail segments) 
    (distGates, thisWires, thisEbits) = distributeGates thisGates thisHyp thisPart
    (thisGates, thisHyp, thisPart,_,_) = head segments
    (_,         _,       nextPart,_,_) = head $ tail segments
    bindings' = updBindings thisGates bindings
    teleGates = map (\(w,_) -> teleAt w) $ filter (\(w,b) -> bindings' M.! w && b /= nextPart M.! w) $ take nWires $ M.toList thisPart -- Ignore CZ-vertices!
    teleAt w = QGate "teleport" False [w] [] [] False

distributeGates :: [Gate] -> Hypergraph -> Partition -> ([Gate],Int,Int)
distributeGates gates hypergraph partition = (allocateEbits components gatesWithCZs eDic 0, newWires, newEbits)
  where 
    gatesWithCZs = distributeCZs nonlocal gates partition eDic 0
    nonlocal = nonLocalCs partition hypergraph
    components = ebitInfo partition nonlocal
    newEbits = length components `div` 2
    (newWires,eDic) = foldr addToDic (0,M.empty) $ filter isEntangler components
    addToDic (Entangler key _ _) (w,dic) = if key `M.member` dic then (w,dic) else (w+2, M.insert key (-w-1) dic) -- For each new (c,b) we allocate a new pair of negative wires

distributeCZs :: [NonLocalConnection] -> [Gate] -> Partition -> EDic -> Int -> [Gate]
distributeCZs []     gs partition _    prev = gs
distributeCZs (c:cs) gs partition eDic prev = gsInit ++ distributeCZs (drop (length thisCs - 1) cs) (g':tail gsTail) partition eDic pos
  where
    gsInit = take (pos-prev) gs
    gsTail = drop (pos-prev) gs
    g' = connectCZTo thisCs ws partition eDic
    thisCs = c : takeWhile (\(_,_,_,pos',_) -> pos == pos') cs
    (_,_,_,pos,_) = c
    ws = getWires (head gsTail)

connectCZTo :: [NonLocalConnection] -> [Wire] -> Partition -> EDic -> Gate
connectCZTo []     ws partition eDic = QGate "CZ" False [head ws] [] (map (\w -> Signed w True) $ tail ws) False
connectCZTo (c:cs) ws partition eDic = connectCZTo cs ws' partition eDic
  where
    ws' = map (\w -> if w == wire then ebit else w) ws
    (_, wire, sink, pos, _) = c 
    ebit = eDic M.! (wire, partition M.! sink) - 1
    
-- Note: The order how the components are added is essential. It must be from the end to the beginning.
allocateEbits :: [EbitComponent] -> [Gate] -> EDic -> Int -> [Gate]
allocateEbits []     gates eDic _    = gates
allocateEbits (c:cs) gates eDic prev = gatesInit ++ component ++ allocateEbits cs gatesTail eDic n
  where
    gatesInit = take (n-prev) gates
    gatesTail = drop (n-prev) gates
    n = position c
    ((source,bsink), bsource) = getConnections c
    sourceE = eDic M.! (source, bsink) -- eBit wire for hyperedge 'source' (i.e. control if control type, target otherwise)
    sinkE = sourceE-1
    component = if isEntangler c
      then bell ++ [QGate "not" False [sourceE] [] [Signed source True] False, QMeas sourceE, QGate "X" False [sinkE] [] [Signed sourceE True] False, Comment "QPU_allocation" False [(sourceE,"-1 ebit")], CDiscard sourceE]
      else [QGate "H" False [sinkE] [] [] False, QMeas sinkE, QGate "Z" False [source] [] [Signed sinkE True] False, Comment "QPU_allocation" False [(sinkE,"-1 ebit")], CDiscard sinkE]
    bell = [QInit False sinkE False,  QInit False sourceE False, Comment "QPU_allocation" False [(sinkE, show bsink ++ " ebit"), (sourceE, show bsource ++ " ebit")], QGate "bell" False [sinkE,sourceE] [] [] False]
    -- explicitBell = [QInit False sinkE False, QInit False sourceE False, QGate "H" False [sourceE] [] [] False, QGate "not" False [sinkE] [] [Signed sourceE True] False]

-- Produces an ordered list of the components to realise the required ebits (cat-ent/disentanglers). The order is given by ascending position in the circuit.
ebitInfo :: Partition -> [NonLocalConnection] -> [EbitComponent]
ebitInfo partition nonlocal = sort $ disentanglers ++ entanglers
  where
    entanglers    = map (\(n,c,b,_) -> Entangler    (c,b) (partition M.! c) n) eInfo
    disentanglers = map (\(_,c,b,n) -> Disentangler (c,b) (partition M.! c) n) eInfo
    eInfo = nub $ map (\(i,c,w,_,o) -> (i,c,partition M.! w,o)) nonlocal -- This nub makes sure there's only one element per cut between gate positions [i,o]

nonLocalCs :: Partition -> Hypergraph -> [NonLocalConnection]
nonLocalCs partition hyp = sortBy (\(_,_,_,pos1,_) (_,_,_,pos2,_) -> compare pos1 pos2) nonlocal
  where
    nonlocal = filter (\(_,src,snk,_,_) -> partition M.! src /= partition M.! snk) czs
    czs      = foldr (\(i,v,ws,o) cs -> map (\(w,p) -> (i,v,w,p,o)) ws ++ cs) [] hedges
    hedges   = M.foldrWithKey (\v vss hs -> map (\(i,ws,o) -> (i,v,ws,o)) vss ++ hs) [] hyp

addPartComments :: BindingFlags -> Partition -> [Gate] -> [Gate]
addPartComments bindings part gates = partComment : gates'
  where
    partComment = Comment "QPU_allocation" False $ [(w,(show $ part M.! w) ++ " QPU") | w <- bound]
    bound = map fst $ filter snd $ M.toList bindings
    gates' = commentOn gates 
    commentOn []     = []
    commentOn (g:gs) = case targetOf g of 
        Just w -> if w `M.member` bindings && w `elem` createdBy g -- Add a comment determining the wire's QPU every time its qubit is initialized.
          then g : (Comment "QPU_allocation" False [(w, (show $ part M.! w) ++ " QPU")]) : commentOn  gs
          else g : commentOn gs
        Nothing -> g : commentOn gs    
    createdBy gate = (\(ins,outs) -> filterQbit $ outs \\ ins) $ gate_arity gate
    filterQbit arity = map fst $ filter (\(_,t) -> case t of Qbit -> True; _ -> False) arity