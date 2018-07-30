
import Data.List (sortBy, sort)
import qualified Data.Map as M

type Wire = Int


getNonLocal hypFile partFile = nonLocal
  where
    partition = map read $ lines partFile
    (headHyp:hyp) = lines hypFile
    [nEdges,nVertices, _] = map read $ words headHyp
    edges = map ((\(src:sinks) -> (src,sinks)) . map read . words) $ take nEdges hyp :: [(Wire,[Wire])]
    edges' = merge $ sortBy (\(a,_) (b,_) -> compare a b) edges :: [(Wire,[Wire])]
      where
        merge (e:[])   = [e]
        merge (e:x:es) = if fst e == fst x 
          then merge ((fst e, snd x ++ snd e):es)
          else e : merge (x:es)
    edges'' = map (\(src,sinks) -> (src, count $ sort sinks)) edges' :: [(Wire,[(Wire,Int)])]
      where
        count [] = []
        count (x:ls) = let (xs,ls') = break (/=x) ls in (x,1+length xs) : count ls
    cnots = concat $ map (\(src,sinks) -> map (\(snk,n) -> (src,snk,n)) sinks) edges'' :: [(Wire,Wire,Int)]
    nonLocal = foldr (\(_,_,n) ac -> n+ac) 0 $ filter (\(p1,p2,_) -> p1/=p2) $ allocate cnots partition 0 :: Int
      where
        allocate cs []     _ = cs
        allocate cs (x:xs) i = allocate (map (\(a,b,n) -> (a',b',n)) cs) xs (i+1)
          where 
            a' = if a==i then (-x) else a
            b' = if b==i then (-x) else b

main = do
  hypFile <- readFile "hypergraph.hgr"
  partFile <- readFile "partition.hgr"
  putStrLn $ "Non-local CNOTs: " ++ (show $ getNonLocal hypFile partFile)

