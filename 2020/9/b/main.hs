stepper :: [Int] -> Int -> Int -> Int
stepper xs p c
  | p < c = stepper xs (p+1) c
  | checkSum x cs == False = x
  | otherwise = stepper xs (p+1) c
  where x = xs !! p
        preamble = take c $ drop (length xs - p) $ reverse xs
        cs = combinations 2 preamble

checkSum :: Int -> [[Int]] -> Bool
checkSum x cs
  | cs == [] = False
  | sum (head cs) /= x = checkSum x (tail cs)
  | otherwise = True

findWeakness :: Int -> Int ->  Int -> [Int] -> Int
findWeakness s c x xs 
  | sum set == x = mi + ma 
  | sum set > x = findWeakness (s+1) 2 x xs 
  | otherwise = findWeakness s (c+1) x xs
  where set = drop s $ take c $ xs
        mi = foldl1 min set
        ma = foldl1 max set

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]
main = do
  input <- readFile "./input.txt"
  let ints = map (\x -> read x :: Int) $ lines input
  let weakness = stepper ints 0 25
  putStr $ show $ findWeakness 0 2 weakness ints

