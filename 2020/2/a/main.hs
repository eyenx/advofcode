import Data.List.Split as Split

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

isPassValid :: [String] -> Bool
isPassValid [x,y,z]
  | checkmax < count = False
  | checkmin > count = False
  | otherwise = True
  where checkmax = (read $ Split.splitOn "-" x !! 1 :: Int) 
        checkmin = (read $ Split.splitOn "-" x !! 0 :: Int) 
        count =  countLetters z c
        c = y !! 0

main = do
  input <- readFile "./input.txt"
  let passwords = map (\x -> Split.splitOn " " x) $ lines input
  let results = map (\x -> isPassValid x) passwords
  putStr $ show $ length $ filter (== True) results
