import Data.List
import Data.List.Split

findItemInList :: Char -> [Char] -> Bool
findItemInList c [] = False
findItemInList c (x:z)
  | c == x = True
  | otherwise = findItemInList c z 

findItems :: (String,String,String) -> Char
findItems ([],y,z) = '0'
findItems (x,[],z) = '0'
findItems (x,y,[]) = '0'
findItems (a:x,y,z) 
  | findItemInList a y && findItemInList a z  = a
  | otherwise = findItems (x,y,z)

calcPrio :: Char -> [Char] -> Int
calcPrio '0' _ = 0 
calcPrio x [] = 0
calcPrio x l = (+1) $ (\(Just i)->i) . elemIndex x $ l

transformToInt :: String -> (String,String,String) -> Int
transformToInt [] _ = 0
transformToInt _ ([],[],[]) = 0
transformToInt x y = calcPrio (findItems y) x

toTuple :: [String] -> (String,String,String)
toTuple x = (head x,x!!1,x!!2)


main = do
 input <- readFile "./input"
 let rucksacks = map toTuple $ chunksOf 3 $ lines input
 let priolist = ['a'..'z']++['A'..'Z']
 putStr $ show $ sum $ map (transformToInt priolist) rucksacks
