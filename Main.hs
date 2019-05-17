import qualified Data.Map as M
import Data.List (sort, sortBy, nub)
import Numeric (showFFloat)
import System.Directory

import Quipper
import Quipper.Circuit
import Quipper.Generic

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.Preparation
import Distributer.Partitioner
import Distributer.HGraphBuilder
import Distributer.DCircBuilder
import Distributer.Examples

prepareTempDirectory :: IO ()
prepareTempDirectory = do 
  dirExists <- doesDirectoryExist "temp"
  if dirExists then removeDirectoryRecursive "temp" else return ()
  createDirectory "temp"

main = do
  (input,shape, name) <- Cfg.circuit
  prepareTempDirectory
  let
    k = show Cfg.k; epsilon = showFFloat (Just 2) Cfg.epsilon ""; mode = (Cfg.pullCNOTs, Cfg.bothRemotes)
    circ  = prepareCircuit input shape mode
    (qin, ((ain,theGates,aout,nWires),namespace), qout) = encapsulate_generic id circ shape
    segments = partitioner theGates nWires mode
    gateCountInput = length theGates
    cnotsInput = length $ filter isCNOT theGates
    (newGates, newWires, nEbits) = buildCircuit theGates nWires segments
    newCircuit = unencapsulate_generic (qin, ((ain,newGates,aout,nWires+newWires),namespace), qout)
    in do
      putStrLn $ ""
--      putStrLn $ "Original circuit:"
--      print_generic Cfg.outputAs input shape
      putStrLn $ ""
      putStrLn $ "After preprocessing:"
      print_generic Cfg.outputAs circ shape
      putStrLn $ ""
      putStrLn $ "New circuit:"
      print_generic Cfg.outputAs newCircuit shape
      putStrLn $ ""
      putStrLn $ "Original gate count: " ++ show gateCountInput
      putStrLn $ "Original CNOT count: " ++ show cnotsInput
      putStrLn $ "Original qubit count: " ++ show nWires
      putStrLn $ ""
      putStrLn $ "Number of non-local CNOTs: " ++ (show $ countNonLocal theGates $ map (\(_,part,pos) -> (part,pos)) segments)
      putStrLn $ "Total number of ebits: " ++ show nEbits
      putStrLn $ ""
      putStrLn $ "Circuit: "++name
      putStrLn $ "Extensions: " ++ (if fst mode then "PullCNOTs (limit: "++show Cfg.pullLimit++"), " else "") ++ (if snd mode then "BothRemotes, " else "")
      putStrLn $ "k = "++show k++"; epsilon = "++show epsilon 
      putStrLn $ "w = "++show Cfg.segmentWindow++"; t = "++show Cfg.testWindow++"; eta = "++show Cfg.tolerance
