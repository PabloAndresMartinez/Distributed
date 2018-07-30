
import Data.List (sortBy, sort)
import qualified Data.Map as M

type Wire = Int

{-

This script calculates the number of non-local CNOTs in a circuit, given its partitioned hypergraph.

I needed this as I wanted to discuss ebit count vs nonlocal count in the thesis, but I did not provide the latter number in the test runs.

Note: it is always possible to do this because, per hyperedge, the "source" (i.e. the wire that has the common role among the CNOTs) is, by construction,
  the first vertex of each hyperedge (in the hypergraph.hgr file).

-}


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
        count (x:ls) = let (xs,ls') = break (/=x) ls in (x,1+length xs) : count ls'
    cnots = concat $ map (\(src,sinks) -> map (\(snk,n) -> (src,snk,n)) sinks) edges'' :: [(Wire,Wire,Int)]
    nonLocal = foldr (\(_,_,n) ac -> n+ac) 0 $ filter (\(p1,p2,_) -> p1/=p2) $ allocate cnots partition 1
      where
        allocate cs []     _ = cs
        allocate cs (x:xs) i = allocate (map upd cs) xs (i+1)
          where 
            upd (a,b,n) = (if a==i then (-x) else a, if b==i then (-x) else b, n)

main = do
  hypFile <- readFile "hypergraph.hgr"
  partFile <- readFile "partition.hgr"
  appendFile "output.txt" $ "\nNon-local CNOTs: " ++ (show $ getNonLocal hypFile partFile)

