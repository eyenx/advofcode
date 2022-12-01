import Data.List.Split

main = do
  input <- readFile "./input"
  print $ foldl max 0 $ map sum $ map (\x -> map (\x -> read x :: Integer) $ endBy "\n" x ) $ endBy "\n\n" input
