import qualified Data.Map as M
import Data.List (sort, sortBy, nub)
import Numeric (showFFloat)

import Quipper
import Quipper.Circuit
import Quipper.Generic

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.Preparation
import Distributer.Distributer
import Distributer.HGraphBuilder
import Distributer.DCircBuilder
import Distributer.Examples


main = do
  (input,shape, name) <- Cfg.circuit
  let
    k = show Cfg.k; epsilon = showFFloat (Just 2) Cfg.epsilon ""; mode = (Cfg.pullCNOTs, Cfg.bothRemotes)
    circ  = prepareCircuit input shape mode
    extractedCirc@(_, ((_,theGates,_,nWires),_), _) = encapsulate_generic id circ shape
    hypergraph = buildHyp theGates nWires mode
    partition = getPartition hypergraph nWires
    gateCountInput = length theGates
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
      putStrLn $ "Circuit: "++name
      putStrLn $ "Extensions: " ++ (if fst mode then "PullCNOTs (limit: "++show Cfg.pullLimit++"), " else "") ++ (if snd mode then "BothRemotes, " else "")
      putStrLn $ "k = "++show k++"; epsilon = "++show epsilon 
