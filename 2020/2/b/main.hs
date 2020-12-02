import Data.List.Split as Split

isPassValid :: [String] -> Bool
isPassValid [x,y,z]
  | z !! checkfst == c && z !! checksnd == c = False
  | z !! checkfst == c || z !! checksnd == c = True
  | otherwise = False
  where checkfst = (read $ Split.splitOn "-" x !! 1 :: Int) -1
        checksnd = (read $ Split.splitOn "-" x !! 0 :: Int) -1 
        c = y !! 0

main = do
  input <- readFile "./input.txt"
  let passwords = map (\x -> Split.splitOn " " x) $ lines input
  let results = map (\x -> isPassValid x) passwords
  putStr $ show $ length $ filter (== True) results
