import qualified Data.Map as M
import Data.List (sort, sortBy, nub)
import qualified HSH as HSH

import Quipper
import Quipper.Circuit
import Quipper.Generic

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.Preparation
import Distributer.HGraphBuilder
import Distributer.DCircBuilder
import Distributer.Examples


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
          partList = map read (concat . map words . lines $ hypPart)
          partition = M.fromList $ zip [0..] partList
          (newCircuit, nEbits) = buildCircuit partition hypergraph extractedCirc
          in do
            putStrLn $ ""
--            putStrLn $ "Original circuit:"
--            print_generic Cfg.outputAs input shape
            putStrLn $ ""
            putStrLn $ "After preprocessing:"
            print_generic Cfg.outputAs circ shape
            putStrLn $ ""
            putStrLn $ "New circuit:"
            print_generic Cfg.outputAs newCircuit shape
            putStrLn $ "Original gate count: " ++ show gateCountInput
            putStrLn $ "Original qubit count: " ++ show nWires
            putStrLn $ "Partition: " ++ show (take nWires $ map snd $ M.toList partition)
            putStrLn $ ""
            putStrLn $ "Number of non-local CNOTs: " ++ (show $ countNonLocal theGates partition)
            putStrLn $ "Total number of ebits: " ++ show nEbits
            putStrLn $ ""
            putStrLn $ "Number of vertices: " ++ show hypVertices
            putStrLn $ "Number of hyperedges: " ++ show hypHEdges
            putStrLn $ ""
            putStrLn $ "Circuit: "++name
            putStrLn $ "Extensions: " ++ (if fst mode then "PullCNOTs (limit: "++show Cfg.pullLimit++"), " else "") ++ (if snd mode then "BothRemotes, " else "")
            putStrLn $ "k = "++show k++"; epsilon = "++show epsilon