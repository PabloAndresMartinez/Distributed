module Distributer.Partitioner where

import qualified Data.Map as M
import Data.List (nub, sort, sortBy, group, (\\))
import Numeric (showFFloat)
import qualified HSH as HSH
import System.IO.Unsafe (unsafePerformIO)

import Quipper
import Quipper.Circuit

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.HGraphBuilder

-- Input is circuit, number of wires and the algorithm mode
-- Output is the list of partitions, indicating the position at which they end and the 'sub'-hypergraph they correspond to.
partitioner :: [Gate] -> Int -> Mode -> [(Hypergraph, Partition, Int)]
partitioner circ nWires mode = matchSegments $ findPartitions circ nWires mode 0
  where
    matchSegments (s:[]) = [s]
    matchSegments (s:ss) = s : matchSegments ss'
      where
        ss' = (h, M.map (\b -> matching M.! b) nextPart, p) : (tail ss) -- Rename blocks of nextPart so they optimally match with those from prevPart
        matching = reverseM $ matchPartitions prevPart nextPart nWires
        reverseM = M.fromList . map (\(a,b) -> (b,a)) . M.toList -- Because matching sends elements of prevPart to nextPart; we need the opposite
        (_,prevPart,_) = s
        (h,nextPart,p) = head ss

findPartitions :: [Gate] -> Int -> Mode -> Int -> [(Hypergraph, Partition, Int)]
findPartitions []   _      _    _       = []
findPartitions circ nWires mode counter = (theHyp,thePart,posR) : findPartitions circR nWires mode (counter+1)
  where
    (theGates, circR) = splitAt posR circ
    theHyp = buildHyp theGates nWires mode
    thePart = getPartition theHyp nWires (show $ counter+1)
    posR = extendSegment testGates wPart rho circ'' nWires mode
    (wGates, circ') = splitAt Cfg.segmentWindow circ
    wHyp = buildHyp wGates nWires mode
    wPart = getPartition wHyp nWires (show counter++".5")
    rho = getRho wHyp wPart
    (testGates, circ'') = splitAt Cfg.testWindow circ'

extendSegment :: [Gate] -> Partition -> Rational -> [Gate] -> Int -> Mode -> Int
extendSegment []    _    _   _    _      _    = Cfg.segmentWindow + Cfg.testWindow
extendSegment gates part rho circ nWires mode = if (1+Cfg.tolerance)*rho < rho' 
    then Cfg.segmentWindow + Cfg.testWindow
    else Cfg.step + extendSegment gates'' part rho circ' nWires mode
  where
    hyp = buildHyp gates nWires mode
    rho' = getRho hyp part
    (gates', circ') = splitAt Cfg.step circ
    gates'' = drop Cfg.step $ gates ++ gates'

getRho :: Hypergraph -> Partition -> Rational
getRho hyp part = toRational nCuts / toRational nHedges
  where
    nHedges = M.foldr (\hs n -> length hs + n) 0 hyp
    nCuts = M.foldr (\c n -> c+n) 0 $ M.mapWithKey countCuts hyp
    countCuts w hs = sum $ map (\(_,ws,_,_) -> (length $ nub $ map (part M.!) $ w : map fst ws) - 1) hs

getPartition :: Hypergraph -> Int -> String -> Partition
getPartition hypergraph nWires id = if head fileData == '0' 
    then error $ "The circuit can be simplified to only use 1-qubit gates. Partitioning is irrelevant."
    else partition
  where
    (fileData, hypHEdges, hypVertices) = hypToString Cfg.algorithm hypergraph nWires
    hypPart = unsafePerformIO $ getPartitionIO fileData id
    partList = map read (concat . map words . lines $ hypPart)
    partition = M.fromList $ zip [0..] partList

getPartitionIO :: String -> String -> IO String
getPartitionIO fileData id = let 
    k = show Cfg.k; epsilon = showFFloat (Just 2) Cfg.epsilon "";
    hypFile  = "temp/hyp_"++id++".hgr"
    partFile = "temp/part_"++id++".hgr"
    script Cfg.Kahypar = Cfg.partDir++"KaHyPar -h "++hypFile++" -k "++k++" -e "++epsilon++" -m direct -o km1 -p "++Cfg.partDir++Cfg.subalgorithm++" -q true"
    script Cfg.Patoh = Cfg.partDir++"PaToH "++hypFile++" "++k++" FI="++epsilon++" UM=O PQ=Q OD=0 PA=13 RA=0 A1=100"
  in do 
    writeFile hypFile $ fileData 
    HSH.run $ script Cfg.algorithm :: IO () 
    HSH.run $ "mv "++hypFile++".part* "++partFile :: IO ()
    readFile partFile

-- ## Heuristic search to match partitions ## --

-- A matching is a map from blocks of the previous partition to blocks of the next partition
type Matching   = M.Map Block Block
type Partition' = M.Map Block [Wire]
type HCost      = M.Map Block Int

matchPartitions :: Partition -> Partition -> Int -> Matching
matchPartitions prevPart nextPart nWires = matchPartitionsRec blocksPrev nextPart' heuristic (M.keys blocksPrev) initial
  where
    initial = [(M.empty, M.foldr (+) 0 heuristic)]
    heuristic = heuristicCost blocksPrev nextPart'
    prevPart' = M.take nWires prevPart -- Take only the wire vertices
    nextPart' = M.take nWires nextPart -- Take only the wire vertices
    blocksPrev = M.foldrWithKey (\w b m -> M.alter (appendIt w) b m) M.empty prevPart'
    appendIt w l = case l of 
      Nothing -> Just $ w:[]
      Just ws -> Just $ w:ws

-- Thanks to lazyness, the recursive search is actually only applied on the best candidates
matchPartitionsRec :: Partition' -> Partition -> HCost -> [Block] -> [(Matching,Int)] -> Matching
matchPartitionsRec blocksPrev nextPart heuristic allBlocks matchings = if M.size (fst best) == length allBlocks -- If the matching with lowest cost is a full solution, stop
    then fst best
    else matchPartitionsRec blocksPrev nextPart heuristic allBlocks (sortThem matchings') 
    -- Sorting is essential; it makes sure that choices whose heuristicCost+actualCost are lowest are searched first
  where
    best = head matchings 
    matchings' = extend best ++ tail matchings -- Only search for the currently best matching
    extend (matching, cost) = [ (M.insert thisBlock b matching, update cost b) 
      | b <- allBlocks \\ M.foldr (:) [] matching ] -- allBlocks - already matched from 'nextPart'
    update cost b = cost - heuristic M.! thisBlock + (length $ filter (/= b) $ map (nextPart M.!) thisWires) -- substitute estimate with real cost
    (thisBlock, thisWires) = (head . M.toList) $ M.difference blocksPrev (fst best) -- Find a block from 'prevPart' not yet allocated
    sortThem = sortBy (\(_,cost) (_,cost') -> compare cost cost')

-- Returns a list of the minimum amount of swaps required to match each block from 'prevPart' to its most similar counterpart in 'nextPart'
heuristicCost :: Partition' -> Partition -> HCost
heuristicCost blocksPrev nextPart = M.mapWithKey hCostFor blocksPrev
  where
    hCostFor b ws = length ws - (minimum $ map length $ (group . sort) $ map (nextPart M.!) ws)
   

