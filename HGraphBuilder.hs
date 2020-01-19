module Distributer.HGraphBuilder where

import qualified Data.Map as M
import Data.List (nub)

import Quipper
import Quipper.Internal.Circuit

import Distributer.Common

buildHyp :: MaxHedgeDist -> Int -> [Gate] -> Hypergraph
buildHyp maxHedgeDist nQubits gs = M.map (filter (\(_,ws,_) -> not $ null ws)) $ M.map splitLongHedges hyp -- Remove all singleton (unconnected) hyperedges
  where
    hyp = toPositive $ buildHypRec gs 0 0 -- Build the hypergraph by exploring the gates recursively
    toPositive = M.map (map (\(i,ws,o) -> (i,map (\(w,p) -> (nQubits-w-1,p)) ws,o)))  -- Convert all negative auxiliary wires to positive ones, so KaHyPart does not explode
    splitLongHedges []            = []
    splitLongHedges ((i,ws,o):hs) = if maxHedgeDist == 1 -- Case where the user is asking to do standard graph partitioning
      then map (\(w,p) -> (p,[(w,p)],p+1)) ws ++ splitLongHedges hs -- This is more efficient than the algorithm below (only usable for this case)
      else if maxHedgeDist < (o-i)
        then let x = splitPos (i,o) in splitLongHedges ((i,takeWhile (before x) ws,x) : (x,dropWhile (before x) ws,o) : hs)
        else (i,ws,o) : splitLongHedges hs
    splitPos (i,o) = i + ((o-i) `div` 2)
    before x (_,p) = p < x

-- The hypergraph is built from the end of the circuit to its start
--  The CZ within the hyperedge and the hyperedges themselves are ordered from earliest in the circuit to latest.
buildHypRec :: [Gate] -> Int -> Int -> Hypergraph
buildHypRec []     _   _  = M.empty
buildHypRec (g:gs) pos czVertex = if isClassical g then buildHypRec gs (pos+1) czVertex
  else case g of
    (QGate "CZ" _ [target] [] signedCtrls _) -> newCZAt (target:controls)
      where
        newCZAt []     = buildHypRec gs (pos+1) (czVertex-1)
        newCZAt (w:ws) = M.alter (newCZ czVertex) w $ newCZAt ws
        newCZ v h = case h of 
          Nothing -> Just [(nan,[(v-1,pos)],pos+1)] -- pos+1 because it finishes AFTER this gate
          Just ((_,ws,o):es) -> Just ((nan,(v-1,pos):ws,o):es) -- Add the czVertex to the last hyperedge group on that wire
        nan = 0
        controls = map from_signed signedCtrls
    _ -> newHedgeAt $ targetOf g
      where
        newHedgeAt (Just wire) = M.alter newHedge wire $ buildHypRec gs (pos+1) czVertex -- Subsequent CZs on 'wire' create a new hyperedge
        newHedgeAt Nothing = error $ "Gate "++show g++" was not properly preprocessed."
        newHedge h = case h of 
          Nothing -> Just [(nan,[],pos)]
          Just ((_,ws,o):es) -> Just ((nan,[],pos):(pos+1,ws,o):es) -- Close the last hyperedge group and create a new empty one
        nan = 0

countCuts :: Segment -> Int
countCuts (_,hyp,part,_,_) = sum $ map (\n -> n-1) blocksPerHedge
  where
    blocksPerHedge = map (length . nub . map (part M.!)) flatData
    flatData = M.foldrWithKey (\v vss hs -> map (\(_,vs,_) -> v:(map (\(w,_) -> w) vs)) vss ++ hs) [] hyp

hypToString :: PartAlg -> Hypergraph -> Int -> (String, Int, Int)
hypToString alg hyp nQubits = (fileData, nHedges, nVertices)
  where    
    flatData = M.foldrWithKey (\v vss hs -> map (\(_,vs,_) -> (v+1):(map (\(w,_) -> w+1) vs)) vss ++ hs) [] hyp
    fileData = (unlines . (map (unwords . (map show)))) $ (fstLine alg) : (map nub flatData) ++ verticesWeights -- map nub flatData is removing repeated vertices within a hedge
    fstLine Kahypar = [nHedges, nVertices, 10]
    fstLine Patoh   = [1, nVertices, nHedges, nPins, 1]
    verticesWeights = [[1] | _ <- [1..nQubits]] ++ [[0] | _ <- [(nQubits+1)..nVertices]]
    nVertices = foldr (max . foldr max 0) 0 flatData
    nHedges   = length flatData
    nPins     = nHedges + (M.foldr (+) 0 $ M.map (sum . map (\(_,vs,_) -> length vs)) hyp )
