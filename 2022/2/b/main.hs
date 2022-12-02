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
calcLetter _ = 0

calcChoice :: [String] -> Int
calcChoice ["A","X"] = 3 + winDrawLose [calcLetter "A",3]
calcChoice ["B","X"] = 1 + winDrawLose [calcLetter "B",1] 
calcChoice ["C","X"] = 2 + winDrawLose [calcLetter "C",2]
calcChoice ["A","Y"] = 1 + winDrawLose [calcLetter "A",1]
calcChoice ["B","Y"] = 2 + winDrawLose [calcLetter "B",2]
calcChoice ["C","Y"] = 3 + winDrawLose [calcLetter "C",3]
calcChoice ["A","Z"] = 2 + winDrawLose [calcLetter "A",2]
calcChoice ["B","Z"] = 3 + winDrawLose [calcLetter "B",3]
calcChoice ["C","Z"] = 1 + winDrawLose [calcLetter "Z",1]

main = do
 input <- readFile "./input"
 let rounds = map (splitOn " ") $ lines input
 putStr $ show $ sum $ map calcChoice rounds
