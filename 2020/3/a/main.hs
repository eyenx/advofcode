isTree :: (Int,String) -> Bool
isTree (x,"") = False
isTree (x,xs)
  | rs !! x  == '#' = True
  | otherwise = False
  where rs = concat $ repeat xs

main = do
  input <- readFile "./input.txt"
  let linesInput = drop 1 $ lines input
  let treeLines = map isTree $ zip [3,6..((*3) $ length linesInput)] linesInput
  putStr $ show $ length $ filter (== True) treeLines
