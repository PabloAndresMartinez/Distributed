import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List (sort, sortBy, nub, find)
import Numeric (showFFloat)
import System.Directory
import System.Environment

import Quipper.Circuit
import Quipper.Generic hiding (print_generic)

import qualified Distributer.Configuration as Cfg
import Distributer.Common
import Distributer.Preparation
import Distributer.Partitioner
import Distributer.HGraphBuilder
import Distributer.DCircBuilder 
import Distributer.ColorPrinting

processArgs :: [String] -> (K, Epsilon, InitSegSize, MaxHedgeDist, KeepCCZ, PartAlg, PartDir, Format)
processArgs []     = (-1, 0.03, 1000, 500, False, Kahypar, "./", GateCount) -- Default values
processArgs (a:as) = case take 3 a of
    "-k=" -> (read $ drop 3 a, e, w, m, kT, alg, dir, o)
    "-e=" -> (k, read $ drop 3 a, w, m, kT, alg, dir, o)
    "-w=" -> (k, e, read $ drop 3 a, m, kT, alg, dir, o)
    "-m=" -> (k, e, w, read $ drop 3 a, kT, alg, dir, o)
    "-cc" -> (k, e, w, m, True, alg, dir, o)
    "-fP" -> (k, e, w, m, kT, Patoh, dir, o)
    "-d=" -> (k, e, w, m, kT, alg, drop 3 a, o)
    "-o=" -> case find (\(tag,_) -> tag == drop 3 a) format_enum of
      Just f -> (k, e, w, m, kT, alg, dir, snd f)
      Nothing -> error $ "Option "++a++" not recognised."
    _     -> error $ "Option "++a++" not recognised."
  where
    (k,e,w,m,kT,alg,dir,o) = processArgs as

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
    (cfg_k,cfg_epsilon,cfg_initSegSize,cfg_maxHedgeDist,cfg_keepCCZ,cfg_partAlg,cfg_partDir,cfg_outputAs) = processArgs args
    (input, shape) = Cfg.circuit
    circ  = prepareCircuit cfg_keepCCZ input shape
    (qin, ((ain,theGates,aout,nWires),namespace), qout) = encapsulate_generic id circ shape
    segments = partitioner (cfg_k, cfg_epsilon, cfg_initSegSize, cfg_maxHedgeDist, cfg_partAlg, cfg_partDir) nWires theGates
    gateCountInput = length theGates
    czsInput = length $ filter isCZ theGates
    (newGates, newWires, nEbits, nTeleports) = buildCircuit nWires (IM.size ain) segments
    newCircuit = unencapsulate_generic (qin, ((ain,newGates,aout,nWires+newWires),namespace), qout)
    in if cfg_k < 2 then putStrLn "You must indicate a number of partitions larger than one; ./Main -k=2" >> putStrLn "" else do
      putStrLn $ ""
      putStrLn $ "Original circuit:"
      print_generic cfg_outputAs input shape
      putStrLn $ ""
      putStrLn $ "After preprocessing:"
      print_generic cfg_outputAs circ shape
      putStrLn $ ""
      putStrLn $ "New circuit:"
      print_generic cfg_outputAs newCircuit shape
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
      putStrLn $ "Circuit: "++Cfg.circuit_name
      putStrLn $ "Extensions: " ++ if cfg_keepCCZ then "KeepCCZ" else "N/A"
      putStrLn $ "k = "++show cfg_k++"; epsilon = "++showFFloat (Just 2) cfg_epsilon ""
      putStrLn $ "initSegSize = "++show cfg_initSegSize++"; maxHedgeDist = "++show cfg_maxHedgeDist
