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

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]
main = do
  input <- readFile "./input.txt"
  let ints = map (\x -> read x :: Int) $ lines input
  putStr . show  $ stepper ints 0 25
