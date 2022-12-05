import Data.List.Split
import Data.Char

readInt :: String -> Int
readInt "" = 0
readInt x = read x :: Int

getRange :: String -> [Int]
getRange "" = []
getRange x = [(readInt $ head $ splitOn "-" x)..(readInt $ (splitOn "-" x)!!1)]

contains :: [Int] -> [Int] -> Bool
contains [] z = False
contains (x:y) z
  | x `elem` z = True
  | otherwise = contains y z

transformLine :: String -> [[Int]]
transformLine "" = [] 
transformLine x = map getRange $ splitOn "," x

calculateLine :: [[Int]] -> Int
calculateLine [] = 0
calculateLine x
  | contains (head x) (x!!1) =  1
  | contains (x!!1) (head x) =  1
  | otherwise = 0

main = do
  input <- readFile "./input"
  putStr $ show $ sum $ map (calculateLine . transformLine) $ lines input
