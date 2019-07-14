{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List (sort, sortBy, nub, find)
import Numeric (showFFloat)
import System.Directory
import System.Environment
import System.IO

import Quipper.Circuit
import Quipper.Generic
import Quipper.Printing
import QuipperLib.QuipperASCIIParser

import Distributer.Common
import Distributer.Preparation
import Distributer.Partitioner
import Distributer.HGraphBuilder
import Distributer.DCircBuilder 
import Distributer.ColorPrinting

processArgs :: [String] -> (K, Size, InitSegSize, MaxHedgeDist, KeepCCZ, PartAlg, PartDir, Format, SaveTrace)
processArgs []     = (-1, -1, 1000, 100, False, Kahypar, "./", GateCount, False) -- Default values
processArgs (a:as) = case take 3 a of
    "-k=" -> (read $ drop 3 a, s, w, m, kT, alg, dir, o, sT)
    "-s=" -> (k, read $ drop 3 a, w, m, kT, alg, dir, o, sT)
    "-w=" -> (k, s, read $ drop 3 a, m, kT, alg, dir, o, sT)
    "-m=" -> (k, s, w, read $ drop 3 a, kT, alg, dir, o, sT)
    "-cc" -> (k, s, w, m, True, alg, dir, o, sT)
    "-fp" -> (k, s, w, m, kT, Patoh, dir, o, sT)
    "-d=" -> (k, s, w, m, kT, alg, drop 3 a, o, sT)
    "-o=" -> case find (\(tag,_) -> tag == drop 3 a) format_enum of
      Just f -> (k, s, w, m, kT, alg, dir, snd f, sT)
      Nothing -> error $ "Option "++a++" not recognised."
    "-st" -> (k, s, w, m, kT, alg, dir, o, True)
    _     -> error $ "Option "++a++" not recognised."
  where
    (k,s,w,m,kT,alg,dir,o,sT) = processArgs as

prepareTempDirectory :: IO ()
prepareTempDirectory = do 
  dirExists <- doesDirectoryExist "temp"
  if dirExists then removeDirectoryRecursive "temp" else return ()
  createDirectory "temp"

main :: IO ()
main = do
  args <- getArgs
  circASCII <- hGetContents stdin
  prepareTempDirectory
  let
    (cfg_k,cfg_s,cfg_initSegSize,cfg_maxHedgeDist,cfg_keepCCZ,cfg_partAlg,cfg_partDir,cfg_outputAs,cfg_saveTrace) = processArgs args
    (shape, input) = parse_circuit circASCII
    circ  = prepareCircuit cfg_keepCCZ input shape
    (qin, ((ain,theGates,aout,nWires),namespace), qout) = encapsulate_generic id circ shape
    nQubits = cfg_k * cfg_s
    segments = partitioner (cfg_k, cfg_initSegSize, cfg_maxHedgeDist, cfg_partAlg, cfg_partDir, cfg_saveTrace) (nQubits,nWires) theGates
    gateCountInput = length theGates
    czsInput = length $ filter isCZ theGates
    (newGates, newWires, nEbits, nTeleports) = buildCircuit nWires (IM.size ain) segments
    newCircuit = unencapsulate_generic (qin, ((ain,newGates,aout,nWires+newWires),namespace), qout)
    printIt c s = case cfg_outputAs of
      Preview -> preview_withColor c s
      _ -> print_generic cfg_outputAs c s
    in if cfg_k < 2 || cfg_s < 1 
      then putStrLn "You must indicate the number of QPUs (k > 1) and their workspace qubit capacity (s > 0); example ./Main -k=2 -s=4" >> putStrLn "" 
      else if nQubits < nWires then putStrLn ("There are not enough qubits to run the circuit. Qubits required: "++show nWires++".") else do
        putStrLn $ ""
        putStrLn $ "Original circuit:"
        printIt input shape
        putStrLn $ ""
        putStrLn $ "After preprocessing:"
        printIt circ shape
        putStrLn $ ""
        putStrLn $ "New circuit:"
        printIt newCircuit shape
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
        putStrLn $ "Extensions: " ++ if cfg_keepCCZ then "KeepCCZ" else "N/A"
        putStrLn $ "#QPUs = "++show cfg_k++"; QPU_size = "++show cfg_s
        putStrLn $ "initSegSize = "++show cfg_initSegSize++"; maxHedgeDist = "++show cfg_maxHedgeDist
