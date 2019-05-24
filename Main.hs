import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List (sort, sortBy, nub)
import Numeric (showFFloat)
import System.Directory
import System.Environment

import Quipper
import Quipper.Circuit
import Quipper.Generic

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.Preparation
import Distributer.Partitioner
import Distributer.HGraphBuilder
import Distributer.DCircBuilder 

processArgs :: [String] -> (K, Epsilon, InitSegSize, MaxHedgeDist, KeepToffoli, PartAlg, PartDir, OutputPreview)
processArgs []     = (-1, 0.03, 1000, 500, False, Kahypar, "./", False) -- Default values
processArgs (a:as) = case take 3 a of
    "-k=" -> (read $ drop 3 a, e, w, m, kT, alg, dir, vi)
    "-e=" -> (k, read $ drop 3 a, w, m, kT, alg, dir, vi)
    "-w=" -> (k, e, read $ drop 3 a, m, kT, alg, dir, vi)
    "-m=" -> (k, e, w, read $ drop 3 a, kT, alg, dir, vi)
    "-kT" -> (k, e, w, m, True, alg, dir, vi)
    "-fP" -> (k, e, w, m, kT, Patoh, dir, vi)
    "-d=" -> (k, e, w, m, kT, alg, drop 3 a, vi)
    "-vi" -> (k, e, w, m, kT, alg, dir, True)
  where
    (k,e,w,m,kT,alg,dir,vi) = processArgs as

prepareTempDirectory :: IO ()
prepareTempDirectory = do 
  dirExists <- doesDirectoryExist "temp"
  if dirExists then removeDirectoryRecursive "temp" else return ()
  createDirectory "temp"

main :: IO ()
main = do
  args <- getArgs
  prepareTempDirectory
  let
    (cfg_k, cfg_epsilon, cfg_initSegSize, cfg_maxHedgeDist, cfg_keepToffoli, cfg_partAlg, cfg_partDir, cfg_outputAs) = processArgs args
    (input, shape) = Cfg.circuit
    circ  = prepareCircuit cfg_keepToffoli input shape
    (qin, ((ain,theGates,aout,nWires),namespace), qout) = encapsulate_generic id circ shape
    segments = partitioner (cfg_k, cfg_epsilon, cfg_initSegSize, cfg_maxHedgeDist, cfg_partAlg, cfg_partDir) nWires theGates
    gateCountInput = length theGates
    czsInput = length $ filter isCZ theGates
    (newGates, newWires, nEbits, nTeleports) = buildCircuit nWires (IM.size ain) segments
    newCircuit = unencapsulate_generic (qin, ((ain,newGates,aout,nWires+newWires),namespace), qout)
    output_format = if cfg_outputAs then Preview else GateCount
    in if cfg_k < 2 then putStrLn "You must indicate a number of partitions larger than one; ./Main -k=2" >> putStrLn "" else do
      putStrLn $ ""
      putStrLn $ "Original circuit:"
      print_generic output_format input shape
      putStrLn $ ""
      putStrLn $ "After preprocessing:"
      print_generic output_format circ shape
      putStrLn $ ""
      putStrLn $ "New circuit:"
      print_generic output_format newCircuit shape
      putStrLn $ ""
      putStrLn $ "Original gate count: " ++ show gateCountInput
      putStrLn $ "Original CZ count: " ++ show czsInput
      putStrLn $ "Original qubit count: " ++ show nWires
      putStrLn $ ""
      putStrLn $ "Number of nonlocal CZs: " ++ (show $ countNonLocal segments)
      putStrLn $ "Number of ebits due nonlocal CZs: " ++ show nEbits
      putStrLn $ "Number of ebits due to teleportations: " ++ show nTeleports
      putStrLn $ "Total number of ebits: " ++ show (nEbits+nTeleports)
      putStrLn $ ""
      --putStrLn $ "Circuit: "++cfg_circuit
      putStrLn $ "Extensions: " ++ if cfg_keepToffoli then "KeepToffoli" else "N/A"
      putStrLn $ "k = "++show cfg_k++"; epsilon = "++showFFloat (Just 2) cfg_epsilon ""
      putStrLn $ "initSegSize = "++show cfg_initSegSize++"; maxHedgeDist = "++show cfg_maxHedgeDist
