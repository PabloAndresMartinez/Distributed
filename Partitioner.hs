module Distributer.Partitioner where

import qualified Data.Map as M
import Data.List (nub, sort, sortBy, group, (\\))
import Numeric (showFFloat)
import System.Process
import System.Directory
import System.IO.Unsafe (unsafePerformIO)

import Quipper
import Quipper.Internal.Circuit

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.HGraphBuilder

-- Input is the circuit and the number of wires
-- Output is the list of partitions, indicating the position at which they end and the 'sub'-hypergraph they correspond to.
partitioner :: (K,InitSegSize,MaxHedgeDist,PartAlg,PartDir,SaveTrace,Verbose) -> (Int,Int) -> [Gate] -> [Segment]
partitioner (k,w,m,alg,dir,sT,vb) (nQubits,nWires) circ = mergeSeams toHypergraph toPartition k nWires $ ignoreLastSeam $ initSegs
  where    
    toHypergraph = buildHyp m nQubits
    toPartition = getPartition (k,alg,dir,sT,vb) nQubits (length initSegs)
    initSegs = initialSegments circ 0
    initialSegments []    _ = []
    initialSegments gates n = segmentOf thisGates n : initialSegments nextGates (n+1)
      where 
        segmentOf gs n = (\hyp -> (gs, hyp, toPartition hyp (show n), Compute, (n,n) )) $ toHypergraph gs
        (thisGates, nextGates) = splitAt (seamPos w gates) gates
        seamPos _ [] = 0
        seamPos 0 gs =  length $ takeWhile (not . isCZ) gs
        seamPos n gs = (length $ takeWhile (not . isCZ) gs) + 1 + seamPos (n-1) (drop 1 $ dropWhile (not . isCZ) gs)

mergeSeams :: ([Gate] -> Hypergraph) -> (Hypergraph -> String -> Partition) -> K -> Int -> [Segment] -> [Segment]
mergeSeams toHypergraph toPartition k nWires segments = if allStop then segments else segments''
  where
    segments'' = mergeSeams toHypergraph toPartition k nWires segments'
    segments' = ignoreLastSeam $ mergeMin toHypergraph toPartition nWires $ computeNewSeams nWires $ matchSegments k nWires idMatching segments
    allStop = foldr (&&) True $ map (\(_,_,_,seam,_) -> isStop seam) segments
    idMatching = M.fromList [(b,b) | b <- [0..k-1]]

-- The last segment has no next segment, so it has no seam.
ignoreLastSeam :: [Segment] -> [Segment]
ignoreLastSeam (s:[]) = (\(gs,hyp,part,_,id) -> [(gs,hyp,part,Stop,id)]) s 
ignoreLastSeam (s:ss) = s : ignoreLastSeam ss

-- Rename blocks so that they match optimally between seams.
matchSegments :: K -> Int -> Matching -> [Segment] -> [Segment]
matchSegments k nWires prevMatching (s:[]) = updWith prevMatching s : []
matchSegments k nWires prevMatching (s:ss) = s' : matchSegments k nWires matching ss 
  where
    s' = updWith prevMatching s -- Rename blocks of this segment's partition so they optimally match with those from the previous segment
    matching = case thisSeam of -- Only compute matchings when necessary (i.e. when this segment or the next one have changed)
      Compute -> updatedMatching
      _       -> case nextSeam of
        Compute -> updatedMatching
        _       -> prevMatching 
    updatedMatching = matchPartitions nextPart thisPart k nWires -- This gives a matching from blocks of nextPart to blocks of thisPart
    (_,_,thisPart, thisSeam,_) = s' -- Note that it is important that the prevMatching has already been applied to the segment
    (_,_,nextPart, nextSeam,_) = head ss

computeNewSeams :: Int -> [Segment] -> [Segment]
computeNewSeams nWires []     = []
computeNewSeams nWires (s:ss) = (gs,hyp,part,seam',id) : computeNewSeams nWires ss
  where
    (gs,hyp,part,seam,id) = s
    seam' = case seam of
      Compute -> Value $ getRho nWires s (head ss)
      _       -> seam

-- A rational number from 0 to 1. A low value indicates that the two partitions are similar, so the segments should likely be merged.
--    The value is calculated as a weighted sum of the wires that require teleportation, the weight depends on how busy each wire is.
--    The minimum of the wireHedges/totalHedges ratio between hyp1 and hyp2 of each wire is taken, 
--      because that is a good estimate of the added cost if the teleportation is not applied
--    Wires that stay in the same block or don't exist in either of the segments are filtered out by filterStatic
getRho :: Int -> Segment -> Segment -> Rational
getRho nWires (_, hyp1, part1,_,_) (_, hyp2, part2,_,_) = sum $ map chooseWeight $ filterStatic [0..nWires-1]
  where
    filterStatic = filter (\w -> M.member w part1 && M.member w part2 && part1 M.! w /= part2 M.! w)
    chooseWeight w = if hedges w hyp1 < hedges w hyp2 then weight w hyp1 else weight w hyp2
    weight wire hyp = toRational (hedges wire hyp) / toRational (totalHs hyp)
    hedges wire hyp = if M.member wire hyp then length $ hyp M.! wire else 0
    totalHs hyp = M.foldr (\hs n -> length hs + n) 0 hyp

-- Finds minimums in the rho sequence and merges the two corresponding segments.
--    It then checks if the created segment has a lower ebit count; if not it restores the original segments and marks the seam as Stop
mergeMin :: ([Gate] -> Hypergraph) -> (Hypergraph -> String -> Partition) -> Int -> [Segment] -> [Segment]
mergeMin toHypergraph toPartition nWires []       = []
mergeMin toHypergraph toPartition nWires segments = case seamOf $ head segments of
    Stop    -> head segments : (mergeMin toHypergraph toPartition nWires $ tail segments)
    Value _ -> valley' ++ mergeMin toHypergraph toPartition nWires segments'
    _       -> error "Error when merging segments."
  where
    valley' = if countCuts leftSeg + countCuts rightSeg + countTeles partLeft partRight < countCuts mergedSeg 
      then map (\(gs,hyp,part,_,id) -> (gs,hyp,part,Stop,id)) (beforeMin++afterMin)
      else beforeMin ++ mergedSeg : (drop 2 afterMin) -- Remove from afterMin the two segments that have been merged
    mergedSeg = (mergedGates, mergedHyp, mergedPart, Compute, (fst idLeft, snd idRight) )
    mergedPart = toPartition mergedHyp $ (show $ fst idLeft) ++ "_" ++ (show $ snd idRight)
    mergedHyp = toHypergraph mergedGates
    mergedGates = gsLeft++gsRight
    leftSeg@ (gsLeft, _,partLeft, _,idLeft)  = head afterMin
    rightSeg@(gsRight,_,partRight,_,idRight) = head $ tail afterMin -- Always exists, as there's always another segment after the minimum (i.e. the one it's seam refers to)
    (beforeMin, afterMin, segments') = findValley segments
    countTeles part1 part2 = length $ filter (\w -> M.member w part1 && M.member w part2 && part1 M.! w /= part2 M.! w) [0..nWires-1]

-- Finds the next minimum seam and its surrounding segments up to a peak on the seam value or a stop.
--    Outputs the segments up to the one with the minimum seam (excluding it), the ones after (including it) up to the end of the valley, and the rest.
findValley :: [Segment] -> ([Segment],[Segment],[Segment])
findValley segments = (beforeMin, afterMin, segments')
  where
    (beforeMin, afterMin) = splitAt minPos valley
    (valley, segments') = splitAt (endPos+1) segments
    (minPos, endPos) = findValleyRec segments 0 Nothing

-- Receives the collection of remaining segments, the position of the head segment with respect to the original input list and the current minimum segment
-- It outputs a pair (minPos, endPos) where minPos is the position of the segment with minimum seam and endPos is the end of the valley.
--    The head of the list of segments must exist and its seam to be a Value type; this is verified by mergeMin which is the only function calling it.
--    The second element must also exist, which is satisfied by the fact that the last segment has seam of type Stop by construction.
findValleyRec :: [Segment] -> Int -> Maybe Int -> (Int,Int)
findValleyRec (s:ss) pos Nothing = case seamOf $ head ss of   -- The minimum has not been found yet.
    Value rho' -> if rho < rho' then findValleyRec ss (pos+1) (Just pos) else findValleyRec ss (pos+1) Nothing
    Stop       -> (pos, pos+1)
    _          -> error "Error when merging segments."
  where (Value rho) = seamOf s
findValleyRec (s:ss) pos (Just m) = case seamOf $ head ss of  -- The minimum is known, now find the end.
    Value rho' -> if rho > rho' then (m,pos) else findValleyRec ss (pos+1) (Just m)
    Stop       -> (m,pos+1)
    _          -> error "Error when merging segments."
  where (Value rho) = seamOf s

-- Calls a third party software to solve the hypergraph partitioning problem
getPartition :: (K,PartAlg,PartDir,SaveTrace,Verbose) -> Int -> Int -> Hypergraph -> String -> Partition
getPartition params@(_,algorithm,_,_,_) nQubits nSegments hypergraph id = if head fileData == '0' 
    then error $ "The circuit can be simplified to only use 1-qubit gates. Partitioning is irrelevant."
    else partition
  where
    (fileData, hypHEdges, hypVertices) = hypToString algorithm hypergraph nQubits
    hypPart = unsafePerformIO $ getPartitionIO params fileData id nSegments
    partList = map read (concat . map words . lines $ hypPart)
    partition = M.fromList $ zip [0..] partList

getPartitionIO :: (K,PartAlg,PartDir,SaveTrace,Verbose) -> String -> String -> Int -> IO String
getPartitionIO (k, algorithm, partDir, saveTrace, verbose) fileData id nSegments = let 
    hypFile  = "temp/hyp_"++id++".hgr"
    partFile = "temp/part_"++id++".hgr"
    script Kahypar = partDir++"KaHyPar -h "++hypFile++" -k "++show k++" -e "++Cfg.epsilon++" -m direct -o km1 -p "++partDir++Cfg.subalgorithm++" -w true -q true"
    script Patoh = partDir++"PaToH "++hypFile++" "++show k++" FI="++Cfg.epsilon++" UM=O PQ=Q OD=0 PA=13 RA=0 A1=100" -- If extra mem is needed: A1=100
    waitForFile file = doesFileExist file >>= \yes -> if yes then return () else waitForFile file
  in do 
    writeFile hypFile $ fileData 
    waitForFile hypFile
    createProcess $ shell $ script algorithm++"; mv "++hypFile++".part* "++partFile
    waitForFile partFile
    if verbose then putStrLn $ "Segment " ++ id ++ " of " ++ (show nSegments) else return ()
    output <- readFile partFile
    if saveTrace then return () else removeFile hypFile >> removeFile partFile
    return output

-- ## Heuristic search to match partitions ## --

-- A matching is a map from blocks of the previous partition to blocks of the next partition
type Partition' = M.Map Block [Wire]
type HCost      = M.Map Block Int

-- This returns a matching map from blocks of 'part1' to blocks of 'part2' so their circuits can be joined with the minimum number of teleportations.
--    For an output 'matching', 'part2' should be updated to 'M.map (\b -> matching M.! b) part2'
matchPartitions :: Partition -> Partition -> K -> Int -> Matching
matchPartitions part1 part2 k nWires = matchPartitionsRec blocks1 part2' heuristic (M.keys blocks1) initial
  where
    initial = [(M.empty, M.foldr (+) 0 heuristic)]
    heuristic = heuristicCost blocks1 part2'
    part1' = M.filter (< nWires) part1 -- Take only the wire vertices
    part2' = M.filter (< nWires) part2 -- Take only the wire vertices
    blocks1 = M.foldrWithKey (\w b m -> M.alter (appendIt w) b m) (M.fromList [(b,[]) | b <- [0..k-1]]) part1'
    appendIt w (Just ws) = Just (w:ws)

-- Thanks to lazyness, the recursive search is actually only applied on the best candidates
matchPartitionsRec :: Partition' -> Partition -> HCost -> [Block] -> [(Matching,Int)] -> Matching
matchPartitionsRec blocks1 part2 heuristic allBlocks matchings = if M.size (fst best) == length allBlocks -- If the matching with lowest cost is a full solution, stop
    then fst best
    else matchPartitionsRec blocks1 part2 heuristic allBlocks (sortThem matchings') 
    -- Sorting is essential; it makes sure that choices whose heuristicCost+actualCost are lowest are searched first
  where
    best = head matchings 
    matchings' = extend best ++ tail matchings -- Only search for the currently best matching
    extend (matching, cost) = [ (M.insert thisBlock b matching, update cost b) 
      | b <- allBlocks \\ M.foldr (:) [] matching ] -- allBlocks - already matched from 'part2'
    update cost b = cost - heuristic M.! thisBlock + (length $ filter (\b' -> b' /= Nothing && b' /= Just b) $ map (`M.lookup` part2) thisWires) -- substitute estimate with real cost
    (thisBlock, thisWires) = (head . M.toList) $ M.difference blocks1 (fst best) -- Find a block from 'part1' not yet allocated
    sortThem = sortBy (\(_,cost) (_,cost') -> compare cost cost')

-- Returns a list of the minimum amount of swaps required to match each block from 'part1' to its most similar counterpart in 'part2'
heuristicCost :: Partition' -> Partition -> HCost
heuristicCost blocks1 part2 = M.mapWithKey hCostFor blocks1
  where
    hCostFor b ws = if null ws then 0 else length ws - (minimum $ map length $ (group . sort) $ map (part2 M.!) $ filter (`M.member` part2) ws)
   

