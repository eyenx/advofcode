import Data.List.Split
import Data.List


main = do
  input <- readFile "./input"
  let sumList = sort $ map sum $ map (\x -> map (\x -> read x :: Integer) $ endBy "\n" x ) $ endBy "\n\n" input
  print $ sum $ take 3 $ reverse sumList
