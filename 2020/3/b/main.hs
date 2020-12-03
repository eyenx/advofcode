isTree :: (Int,String) -> Bool
isTree (_,"") = False
isTree (0,_) = False
isTree (x,xs)
  | rs !! x  == '#' = True
  | otherwise = False
  where rs = concat $ repeat xs

slopeCalc :: [String] -> Int -> Int 
slopeCalc _ 0  = 0
slopeCalc [] _ = 0 
slopeCalc x y = length $ filter (== True) $ map isTree $ zip [y,(2*y)..((*y) $ length x)] x 

main = do
  input <- readFile "./input.txt"
  let linesInput = drop 1 $ lines input
  let oddlinesInput = map fst $ filter (odd . snd) $ zip linesInput [2..]
  putStr $ show $ (slopeCalc oddlinesInput 1) * (product $ map (slopeCalc linesInput) [1,3,5,7] )
