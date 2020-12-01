find2020pair :: Integer -> [Integer] -> String
find2020pair x [] = ""
find2020pair x (y:xs) 
  | x + y == 2020 = show $ x*y
  | otherwise = find2020pair x xs

pairList :: [Integer] -> String
pairList [] = ""
pairList (x:[]) = ""
pairList (x:xs)
  | find2020pair x xs /= "" = find2020pair x xs
  | otherwise = pairList xs

main = do
  input <- readFile "./input.txt"
  let iLines = map (\x -> read x :: Integer) $ lines input
  putStr $ pairList iLines
