import Data.List


countEveryone :: [String] -> Int
countEveryone [] = 0
countEveryone (x:xs) = length $ filter (==True) $ map (elemInRest (x:xs)) x

elemInRest :: [String] -> Char -> Bool
elemInRest [] _ = False
elemInRest xs x = foldl1 (&&) $ map (elem x) xs 


main = do
  input <- readFile "./input.txt"
  let groupAnswers = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines $ input
  putStr . show . sum . map countEveryone $ groupAnswers
