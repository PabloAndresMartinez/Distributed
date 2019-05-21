module Distributer.HGraphBuilder where

import qualified Data.Map as M
import Data.List (nub)

import Quipper
import Quipper.Circuit

import qualified Distributer.Configuration as Cfg
import Distributer.Common

buildHyp :: [Gate] -> Int -> Hypergraph
buildHyp gs nWires = M.map (filter (\(_,ws,_) -> not $ null ws)) hyp -- Remove all singleton (unconnected) hyperedges
  where
    hyp = toPositive $ buildHypRec gs 0 0 -- Build the hypergraph by exploring the gates recursively
    toPositive = M.map (map (\(i,ws,o) -> (i,map (\(w,p) -> (nWires-w-1,p)) ws,o)))  -- Convert all negative auxiliary wires to positive ones, so KaHyPart does not explode

-- The hypergraph is built from the end of the circuit to its start
buildHypRec :: [Gate] -> Int -> Int -> Hypergraph
buildHypRec []     _ _  = M.empty
buildHypRec (g:gs) n czVertex = if isClassical g then buildHypRec gs (n+1) czVertex
  else case g of
    (QGate "CZ" _ [target] [] [signedCtrl] _) -> newCZAt control target
      where
        newCZAt ctrl target = M.alter (newCZ czVertex) target $ M.alter (newCZ czVertex) ctrl $ buildHypRec gs (n+1) (czVertex-1)
        newCZ v h = case h of 
          Nothing -> Just [(nan,[(v-1,n)],n+1)] -- n+1 because it finishes AFTER this gate
          Just ((_,ws,o):es) -> Just ((nan,(v-1,n):ws,o):es) -- Add the czVertex to the last hyperedge group on that wire
        nan = 0
        control = getWire signedCtrl
        getWire (Signed w s) = if s then w else error $ "Negative control" -- As Clifford+T only allows positive controls
    _ -> newHedgeAt $ targetOf g
      where
        newHedgeAt wire  = M.alter newHedge wire $ buildHypRec gs (n+1) czVertex -- Subsequent CZs on 'wire' create a new hyperedge
        newHedge h = case h of 
          Nothing -> Just [(nan,[],n)]
          Just ((_,ws,o):es) -> Just ((nan,[],n):(n+1,ws,o):es) -- Close the last hyperedge group and create a new empty one
        nan = 0

hypToString :: Cfg.PartAlg -> Hypergraph -> Int -> (String, Int, Int)
hypToString alg hyp n = (fileData, nHedges, nVertices)
  where    
    flatData = M.foldrWithKey (\v vss hs -> map (\(_,vs,_) -> (v+1):(map (\(w,_) -> w+1) vs)) vss ++ hs) [] hyp
    fileData = (unlines . (map (unwords . (map show)))) $ (fstLine alg) : (map nub flatData) ++ verticesWeights -- map nub flatData is removing repeated vertices within a hedge
    fstLine Cfg.Kahypar = [nHedges, nVertices, 10]
    fstLine Cfg.Patoh   = [1, nVertices, nHedges, (nVertices-n)*2+nHedges, 1]
    verticesWeights = [[1] | _ <- [1..n]] ++ [[0] | _ <- [(n+1)..nVertices]]
    nVertices = foldr (max . foldr max 0) 0 flatData
    nHedges   = length flatData
