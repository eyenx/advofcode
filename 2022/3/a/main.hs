import Data.List

getCompartments :: String -> (String,String)
getCompartments x = splitAt (length x `div` 2) x

findItemInList :: Char -> [Char] -> Bool
findItemInList c [] = False
findItemInList c (x:z)
  | c == x = True
  | otherwise = findItemInList c z 

findItems :: (String,String) -> Char
findItems ([],y) = '0'
findItems (x:z,y) 
  | findItemInList x y = x
  | otherwise = findItems (z,y)

calcPrio :: Char -> [Char] -> Int
calcPrio '0' _ = 0 
calcPrio x [] = 0
calcPrio x l = (+1) $ (\(Just i)->i) . elemIndex x $ l

transformToInt :: String -> String -> Int
transformToInt [] _ = 0
transformToInt _ [] = 0
transformToInt x y = calcPrio (findItems (getCompartments y)) x

main = do
 input <- readFile "./input"
 let rucksacks = lines input
 let priolist = ['a'..'z']++['A'..'Z']
 putStr $ show $ sum $ map (transformToInt priolist) rucksacks
