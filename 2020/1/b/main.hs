-- https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]
                                  

findPairs :: [[Integer]] -> String
findPairs [] = ""
findPairs (x:xs)
  | foldl (+) 0 x == 2020 = show $ foldl (*) 1 x
  | otherwise = findPairs xs

main = do
  input <- readFile "./input.txt"
  let iLines = map (\x -> read x :: Integer) $ lines input
--  putStr $ findPairs $ combinations 2 iLines
  putStr $ findPairs $ combinations 3 iLines
