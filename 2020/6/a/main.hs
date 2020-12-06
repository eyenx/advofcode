import Data.List


main = do
  input <- readFile "./input.txt"
  let groupAnswers = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines $ input
  putStr . show . sum . map (length . nub . concat) $ groupAnswers
