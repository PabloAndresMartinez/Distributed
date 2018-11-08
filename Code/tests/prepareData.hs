
import Numeric (showFFloat)

showF :: Float -> String
showF f = showFFloat (Just 3) f "" 

isK :: Int -> [String] -> Bool
isK n dat = (read $ take 1 $ drop (length circ - 4) circ) == n
  where circ = head dat

main = do
  content <- readFile "data.dat"
  let
    prop x y = read x / read y
    theData = map words $ drop 2 $ lines content
    x3 = map show [1..70]
    data3 = map (\(x, [_,cnots,nonlocal,ebits,_,_,_,_]) -> [x, showF $ prop ebits cnots, showF $ (prop nonlocal cnots - prop ebits cnots)]) $ zip x3 theData
    x4 = map show [1..70]
    data4 = map (\(x, [_,_,_,_,origQ,totalQ,_,_]) -> [x,showF $ (prop totalQ origQ - 1)]) $ zip x4 theData
    in do
    writeFile "data3.dat" $ unlines $ "X EbitsProp NonLocalPropDiff" : (map unwords data3)
    writeFile "data4.dat" $ unlines $ "X PropQ" : (map unwords data4)


