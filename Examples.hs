import Quipper
import Quipper.Generic
import Quipper.Monad
import Quipper.QData
import Quipper.Circuit
import QuipperLib.Decompose
import QuipperLib.Arith
import Algorithms.BF.BooleanFormula
import System.Random
import Libraries.RandomSource
import Libraries.Sampling
import QuipperLib.Unboxing
import Data.IntMap.Strict (IntMap)
import System.IO.Unsafe (unsafePerformIO)

import qualified QuipperLib.QFT as QFT
import qualified Algorithms.BF.BooleanFormula as BF
import qualified Algorithms.BWT.BWT as BWT
import qualified Algorithms.GSE.GSE as GSE
import qualified Algorithms.GSE.GSEData as GSEData
import qualified Algorithms.TF.QWTFP as TF
import qualified Algorithms.TF.Definitions as TFDef
import qualified Algorithms.TF.Oracle as TFOrac
import qualified Algorithms.USV.USV as USV
import qualified Algorithms.USV.Definitions as USVDef

import Quipper.Printing
import System.Environment

main = do
  circName : extraArgs <- getArgs
  case circName of
    "qft"          -> uncurry (print_generic ASCII) $ qft (read $ head extraArgs) 
    "bfWalk"       -> uncurry (print_generic ASCII) $ bfWalk
    "bwt"          -> uncurry (print_generic ASCII) $ bwt
    "gse"          -> uncurry (print_generic ASCII) $ gse
    "tf"           -> uncurry (print_generic ASCII) $ tf
    "usvR"         -> uncurry (print_generic ASCII) $ usvR
    "usvF"         -> uncurry (print_generic ASCII) $ usvF
    "usvG"         -> uncurry (print_generic ASCII) $ usvG
    "usvH"         -> uncurry (print_generic ASCII) $ usvH
    "simple"       -> uncurry (print_generic ASCII) $ simple
    "simple2"      -> uncurry (print_generic ASCII) $ simple2
    "simple3"      -> uncurry (print_generic ASCII) $ simple3
    "simple4"      -> uncurry (print_generic ASCII) $ simple4
    "pull"         -> uncurry (print_generic ASCII) $ pull
    "pull2"        -> uncurry (print_generic ASCII) $ pull2
    "pull3"        -> uncurry (print_generic ASCII) $ pull3
    "interesting"  -> uncurry (print_generic ASCII) $ interesting
    "interesting2" -> uncurry (print_generic ASCII) $ interesting2
    "interesting3" -> uncurry (print_generic ASCII) $ interesting3
    "interesting4" -> uncurry (print_generic ASCII) $ interesting4
    "withToffolis" -> uncurry (print_generic ASCII) $ withToffolis


-- ## Custom circuits ## --

simple :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
simple = (simple_C, (qubit,qubit,qubit,qubit))
simple_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
simple_C (a,b,c,d) = do
  qnot_at d `controlled` b
  qnot_at c `controlled` b
  qnot_at a `controlled` b
  return (a,b,c,d)

simple2 :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
simple2 = (simple2_C, (qubit,qubit,qubit,qubit))
simple2_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
simple2_C (a,b,c,d) = do
  qnot_at d `controlled` b
  qnot_at c `controlled` b
  qnot_at a `controlled` b
  qnot_at b `controlled` a
  gate_H_at a
  gate_H_at a
  qnot_at c `controlled` a
  qnot_at d `controlled` a
  return (a,b,c,d)

simple3 :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
simple3 = (simple3_C, (qubit,qubit,qubit,qubit))
simple3_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
simple3_C (a,b,c,d) = do
  qnot_at a `controlled` b
  hadamard_at b
  hadamard_at a
  qnot_at a `controlled` b
  hadamard_at b
  hadamard_at a
  qnot_at a `controlled` b
  hadamard_at b
  hadamard_at a
  qnot_at b `controlled` c
  hadamard_at c
  hadamard_at d
  qnot_at d `controlled` c
  hadamard_at c
  hadamard_at d
  qnot_at d `controlled` c
  return (a,b,c,d)

simple4 :: ((Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit))
simple4 = (simple4_C, (qubit,qubit,qubit))
simple4_C :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
simple4_C (a,b,c) = do
  qnot_at b `controlled` a
  qnot_at c `controlled` a
  qnot_at c `controlled` b  
  return (a,b,c)

pull :: ((Qubit, Qubit) -> Circ (Qubit, Qubit), (Qubit,Qubit))
pull = (pull_C, (qubit,qubit))
pull_C :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
pull_C (a,b) = do
  qnot_at b `controlled` a
  named_gate_at "X" a
  named_gate_at "Z" b
  qnot_at b `controlled` a
  named_gate_at "X" a
  named_gate_at "Z" b
  qnot_at a `controlled` b
  named_gate_at "X" a
  named_gate_at "Z" b
  return (a,b)

pull2 :: ((Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit), (Qubit,Qubit, Qubit))
pull2 = (pull2_C, (qubit,qubit,qubit))
pull2_C :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
pull2_C (a,b,c) = do
  qnot_at a `controlled` b
  gate_X_at a
  qnot_at b `controlled` a
  gate_H_at b
  qnot_at a `controlled` b
  gate_Y_at b
  qnot_at b `controlled` c
  gate_H_at b
  qnot_at c `controlled` b
  gate_Z_at c
  qnot_at b `controlled` c
  return (a,b,c)

pull3 :: ((Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit), (Qubit,Qubit, Qubit))
pull3 = (pull3_C, (qubit,qubit,qubit))
pull3_C :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
pull3_C (a,b,c) = do
  qnot_at b `controlled` c 
  gate_X_at b
  qnot_at b `controlled` a
  gate_Y_at b
  qnot_at a `controlled` c
  gate_H_at a
  gate_T_at c
  qnot_at b `controlled` c
  qnot_at a `controlled` b
  return (a,b,c)

interesting :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
interesting = (interesting_C, (qubit,qubit,qubit,qubit))
interesting_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting_C (a,b,c,d) = do
  qnot_at d `controlled` a
  qnot_at b `controlled` c
  qnot_at d `controlled` c
  gate_T_at b
  gate_H_at c
  qnot_at b `controlled` a
  qnot_at a `controlled` c
  return (a,b,c,d)

interesting2 :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
interesting2 = (interesting2_C, (qubit,qubit,qubit,qubit))
interesting2_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting2_C  (a,b,c,d) = do
  qnot_at d `controlled` a
  gate_T_at a
  qnot_at c `controlled` a
  qnot_at a `controlled` b
  gate_H_at b
  qnot_at c `controlled` d
  qnot_at d `controlled` b
  qnot_at a `controlled` b
  gate_H_at b
  gate_Y_at d
  qnot_at d `controlled` b
  return (a,b,c,d)

interesting3 :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
interesting3 = (interesting3_C, (qubit,qubit,qubit,qubit))
interesting3_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting3_C (a,b,c,d) = do
  qnot_at d `controlled` a
  gate_S_at a
  gate_Y_at d
  qnot_at b `controlled` a 
  qnot_at d `controlled` c
  gate_H_at a
  qnot_at d `controlled` b
  qnot_at c `controlled` a
  gate_Z_at a
  qnot_at d `controlled` a
  return (a,b,c,d)

interesting4 :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
interesting4 = (interesting4_C, (qubit,qubit,qubit,qubit))
interesting4_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
interesting4_C (a,b,c,d) = do
  qnot_at a `controlled` c
  qnot_at b `controlled` c
  gate_H_at b
  qnot_at c `controlled` d
  qnot_at b `controlled` c
  gate_H_at d
  qnot_at b `controlled` d
  return (a,b,c,d)

withToffolis :: ((Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit), (Qubit,Qubit,Qubit,Qubit))
withToffolis = (withToffolis_C, (qubit,qubit,qubit,qubit))
withToffolis_C :: (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
withToffolis_C (a,b,c,d) = do
  qnot_at a `controlled` c
  qnot_at b `controlled` [c,d]
  gate_H_at b
  qnot_at c `controlled` d
  qnot_at b `controlled` c
  gate_H_at d
  qnot_at b `controlled` [a,d]
  return (a,b,c,d)


-- ## From Quipper ## --

-- Quantum Fourier Transform
qft :: Int -> ([Qubit] -> Circ [Qubit], [Qubit])
qft n = (QFT.qft_rev, [qubit | _ <- [1..n]])

-- BF, only the walk part, using the default test oracle
bfWalk :: (BooleanFormulaRegister -> Circ (), BooleanFormulaRegister)
bfWalk = (BF.walk, BF.registerShape $ BF.test_oracle)

-- BWT with default options
bwt :: (() -> Circ [Bit], ())
bwt = (bwt_C, ())
bwt_C :: () -> Circ [Bit]
bwt_C _ = BWT.qrwbwt oracle s dt
  where
    s = 1 -- number of timesteps
    dt = pi/180 -- timestep size
    oracle = BWT.oracle_orthodox f g -- the default oracle construction for the two given bitstrings
      where
        n = 10 -- tree height (input bitstring length)
        f = take n (True : False : f) -- a function
        g = take n (False : True : g) -- another function
        
-- GSE full circuit with default options
--    Needs to be IO because it loads from some files    
gse :: (() -> Circ [Bit], ())
gse = (unsafePerformIO gse_C, ())
gse_C :: IO (() -> Circ [Bit])
gse_C = let 
  b        = 3 -- The number of precision qubits
  m        = 8 -- The number of basis functions
  occupied = 4 -- The number of occupied orbitals
  delta_e  = 6.5536 -- Energy range
  tau = 2*pi / delta_e -- The Hamiltonian scaling parameter
  e_max    = -3876.941 -- Maximum energy
  nfun     = (\k -> 1)  -- The function k -> Nk; by default, we skip the repetition
  orthodox = False -- Use orthodox Coulomb operator?
  -- Integral data hpq and hpqrs:
  h1_file  = "h_1e_ascii"
  h2_file  = "h_2e_ascii"
  datadir  = "gseData/"
  in do
    gse_data <- GSEData.load_gse_data m (datadir++h1_file) (datadir++h2_file)
    return $ (\_ -> GSE.gse b m occupied gse_data tau e_max nfun orthodox)
    
-- TF full circuit with Orthodox oracle
tf :: (() -> Circ (Bit, TFDef.CNode, IntMap TFDef.CNode, IntMap (IntMap Bit)), ())
tf = (tf_C, ())
tf_C :: () -> Circ (Bit, TFDef.CNode, IntMap TFDef.CNode, IntMap (IntMap Bit))
tf_C _ = TF.a1_QWTFP spec
  where
    spec = (n, r, (\u v edge -> do (u,v,edge) <- TFOrac.o1_ORACLE l u v edge; return edge), TF.standard_qram)
    l = 4
    n = 3
    r = 2

-- USV, circuit for the R algorithm (3)
usvR :: (() -> Circ USVDef.TwoPoint, ())
usvR = (usvR_C, ())
usvR_C :: () -> Circ USVDef.TwoPoint
usvR_C _ = USV.algorithm_R b l m i0 p randomgen
  where
    n = 3
    b = (replicate n (replicate n 1))
    l = ceiling $ USVDef.norm $ head b
    m = p-1
    i0 = 0
    p = USVDef.find_prime ((n_from_b)^3)
    n_from_b = length b
    randomgen = mkStdGen 1234

-- USV, circuit for the F subroutine (f_quantum)
usvF_b = (replicate 2 (replicate 2 1))
usvF :: (USVDef.TwoPoint -> Circ [QDInt], USVDef.TwoPoint)
usvF = (usvF_C,  twopoint_from_b)
  where
    twopoint_from_b = (qubit, (replicate n_from_b (qdint_shape (4*n_from_b))))
    n_from_b = length usvF_b 
usvF_C :: USVDef.TwoPoint -> Circ [QDInt]
usvF_C = USV.f_quantum usvF_b p m i0
  where
    l = ceiling $ USVDef.norm $ head usvF_b 
    m = p-1
    i0 = 0
    p = USVDef.find_prime ((n_from_b)^3)
    n_from_b = length usvF_b 

-- USV, circuit for the G subroutine (g_quantum)
usvG_b = (replicate 2 (replicate 2 1))
usvG_n = 2
usvG :: ([QDInt] -> Circ [QDInt], [QDInt])
usvG = (usvG_C, vector_from_b)
  where
    vector_from_b = (replicate n_from_b (qdint_shape s))
    s = ceiling (logBase 2 (fromIntegral max_b)) + 5*usvG_n
    max_b = maximum (map maximum usvG_b)
    n_from_b = length usvG_b 
usvG_C :: [QDInt] -> Circ [QDInt]
usvG_C = USV.g_quantum (toInteger usvG_n) ws 
  where
    randomgen = mkStdGen 1234
    ws = take usvG_n $ sample_random0 randomgen 1 

-- USV, circuit for the H subroutine (h_quantum)
usvH_n = 5
usvH :: ([QDInt] -> Circ QDInt, [QDInt])
usvH = (usvH_C, vector_from_n)
  where
    vector_from_n = (replicate usvH_n (qdint_shape (4*usvH_n)))
usvH_C :: [QDInt] -> Circ QDInt
usvH_C = USV.h_quantum
