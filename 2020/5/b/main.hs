import Data.List
import Data.List.Split 
import Text.Regex.Posix 


getRow :: String -> Int
getRow "" = 0
getRow x = fst $ foldl walk (0,127) x


getCol :: String -> Int 
getCol "" = 0
getCol x = fst $ foldl walk (0,7) x

walk :: (Int,Int) -> Char -> (Int,Int)
walk (_,0) _ = (0,0) 
walk (_,_) ' ' = (0,0)
walk (x,y) z
  | z == 'F' || z == 'L' = (x,x+fhalf)
  | otherwise =  (x+bhalf,y)
  where bhalf = div (y-x) 2 +1
        fhalf = div (y-x) 2 


passToID :: String -> Int
passToID "" = 0
passToID x = row * 8 + col
  where row = getRow $ take 7 x 
        col = getCol . reverse . take 3 $ reverse x 


findSeat :: [Int] -> Int
findSeat [] = 0
findSeat (x:[]) = 0
findSeat (x:xs)
  | elem (succ x) xs == True = findSeat xs
  | otherwise = succ x


main = do
  input <- readFile "./input.txt"
  let boardingPasses = lines $ input
  putStr . show . findSeat . sort $ map passToID boardingPasses

