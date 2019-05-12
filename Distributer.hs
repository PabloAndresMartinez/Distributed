module Distributer.Distributer where

import qualified Data.Map as M
import Numeric (showFFloat)
import qualified HSH as HSH
import System.IO.Unsafe (unsafePerformIO)

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.HGraphBuilder


getPartition :: Hypergraph -> Int -> Partition
getPartition hypergraph nWires = if head fileData == '0' 
    then error $ "The circuit can be simplified to only use 1-qubit gates. Partitioning is irrelevant."
    else partition
  where
    (fileData, hypHEdges, hypVertices) = hypToString Cfg.algorithm hypergraph nWires
    hypPart = unsafePerformIO $ getPartitionIO fileData
    partList = map read (concat . map words . lines $ hypPart)
    partition = M.fromList $ zip [0..] partList

getPartitionIO :: String -> IO String
getPartitionIO fileData = let 
    k = show Cfg.k; epsilon = showFFloat (Just 2) Cfg.epsilon "";
    script Cfg.Kahypar = Cfg.partDir++"KaHyPar -h hypergraph.hgr -k "++k++" -e "++epsilon++" -m direct -o km1 -p "++Cfg.partDir++Cfg.subalgorithm++" -q true"
    script Cfg.Patoh = Cfg.partDir++"PaToH hypergraph.hgr "++k++" FI="++epsilon++" UM=O PQ=Q OD=0 PA=13 RA=0"
  in do 
    writeFile "hypergraph.hgr" $ fileData 
    HSH.run $ script Cfg.algorithm :: IO () 
    HSH.run $ "mv hypergraph.hgr.part* partition.hgr" :: IO ()
    readFile "partition.hgr"