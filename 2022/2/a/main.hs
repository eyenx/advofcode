import Data.List.Split

winDrawLose :: [Int] -> Int
winDrawLose [x,y] 
  | (x - y) == (-1) = 6
  | (x - y) == 2 = 6
  | (x - y) == 0 = 3
  | otherwise = 0 

calcLetter :: String -> Int
calcLetter "A" = 1
calcLetter "B" = 2
calcLetter "C" = 3
calcLetter "X" = 1
calcLetter "Y" = 2
calcLetter "Z" = 3
calcLetter _ = 0

calculateScore :: [String] -> Int
calculateScore [x,y] = (winDrawLose $ map calcLetter [x,y]) + calcLetter y

main = do
 input <- readFile "./input"
 let rounds = map (splitOn " ") $ lines input
 putStr $ show $ sum $ map calculateScore rounds
