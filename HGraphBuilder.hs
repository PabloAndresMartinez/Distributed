module Distributer.HGraphBuilder where

import qualified Data.Map as M
import Data.List (nub)

import Quipper
import Quipper.Circuit

import qualified Distributer.Configuration as Cfg
import Distributer.Common

buildHyp :: [Gate] -> Int -> Mode -> Hypergraph
buildHyp gs nWires (_, f_both) = M.map (filter (\(_,ws,_,_) -> not $ null ws)) hyp -- Remove all singleton (unconnected) hyperedges
  where
    hyp = if f_both then toPositive (bothHyp gs 0 0) else vanillaHyp gs 0 -- Build the hypergraph by exploring the gates recursively
    toPositive = M.map (map (\(i,ws,o,ht) -> (i,map (\(w,p) -> (nWires-w-1,p)) ws,o,ht)))  -- Convert all negative auxiliary wires to positive ones, so KaHyPart does not explode

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
    flatData = M.foldrWithKey (\v vss hs -> map (\(_,vs,_,_) -> (v+1):(map (\(w,_) -> w+1) vs)) vss ++ hs) [] hyp
    fileData = (unlines . (map (unwords . (map show)))) $ (fstLine alg) : (map nub flatData) ++ verticesWeights -- map nub flatData is removing repeated vertices within a hedge
    fstLine Cfg.Kahypar = [nHedges, nVertices, 10]
    fstLine Cfg.Patoh   = [1, nVertices, nHedges, (nVertices-n)*2+nHedges, 1]
    verticesWeights = [[1] | _ <- [1..n]] ++ [[0] | _ <- [(n+1)..nVertices]]
    nVertices = foldr (max . foldr max 0) 0 flatData
    nHedges   = length flatData
