import Data.List.Split
import Data.Char

readInt :: String -> Int
readInt "" = 0
readInt x = read x :: Int

getRange :: String -> [Int]
getRange "" = []
getRange x = [(readInt $ head $ splitOn "-" x)..(readInt $ (splitOn "-" x)!!1)]

isCompletelyIn :: [Int] -> [Int] -> Bool
isCompletelyIn [] z = True
isCompletelyIn (x:y) z
  | x `elem` z = isCompletelyIn y z
  | otherwise = False

transformLine :: String -> [[Int]]
transformLine "" = [] 
transformLine x = map getRange $ splitOn "," x

calculateLine :: [[Int]] -> Int
calculateLine [] = 0
calculateLine x
  | isCompletelyIn (head x) (x!!1) =  1
  | isCompletelyIn (x!!1) (head x) =  1
  | otherwise = 0

main = do
  input <- readFile "./input"
  putStr $ show $ sum $ map (calculateLine . transformLine) $ lines input
