
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
    x1 = map show [0.7,1.1,0.9,1.3,1.7,2.1,1.9,2.3,2.7,3.1,2.9,3.3,3.7,4.1,3.9,4.3,4.7,5.1,4.9,5.3]
    data1' = (\ds -> take 16 ds ++ drop 32 ds) $ filter (isK 5) theData
    data1 = map (\(x1, [_,cnots,nonlocal,ebits,_,_,_,_]) -> [x1, showF $ prop ebits cnots, showF $ (prop nonlocal cnots - prop ebits cnots)]) $ zip x1 data1'
    x2 = map show [1,2,3,4,5]
    data2'' = (\ds -> drop 16 ds ++ drop 32 ds) $ filter (isK 5) theData
    data2' = map (\[_,_,_,ebits,_,_,_,_] -> ebits) data2''
    data2 = map (\(x,(e1,e2)) -> [x,e1,e2]) $ zip x2 (propList data2')
      where 
        propList [] = []
        propList (m00:m01:m10:m11:ls) = (showF $ prop m01 m00, showF $ prop m11 m00) : propList ls
    x3 = map show [0.7,0.9,1.1,1.3,1.7,1.9,2.1,2.3,2.7,2.9,3.1,3.3,3.7,3.9,4.1,4.3,4.7,4.9,5.1,5.3]
    data3' = map (theData !!) [2,6,10,14,18,22,26,30,33,37,41,45,51,55,59,63,129,133,137,141]
    data3 = map (\(x, [_,cnots,nonlocal,ebits,_,_,_,_]) -> [x, showF $ prop ebits cnots, showF $ (prop nonlocal cnots - prop ebits cnots)]) $ zip x3 data3'
    x4 = map show [0.7,0.9,1.1,1.3,1.7,1.9,2.1,2.3,2.7,2.9,3.1,3.3,3.7,3.9,4.1,4.3,4.7,4.9,5.1,5.3]
    data4' = map (theData !!) [2,6,10,14,18,22,26,30,33,37,41,45,51,55,59,63,129,133,137,141]
    data4 = map (\(x, [_,_,_,_,origQ,totalQ,_,_]) -> [x,showF $ (prop totalQ origQ - 1)]) $ zip x4 data4'
    in do
    writeFile "data1.dat" $ unlines $ "X EbitsProp NonLocalPropDiff" : (map unwords data1)
    writeFile "data2.dat" $ unlines $ "X Ebits01 Ebits11" : (map unwords data2)
    writeFile "data3.dat" $ unlines $ "X EbitsProp NonLocalPropDiff" : (map unwords data3)
    writeFile "data4.dat" $ unlines $ "X PropQ" : (map unwords data4)