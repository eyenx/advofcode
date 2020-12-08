import Data.List
import Data.List.Split

-- Couldn't get it - help with https://dev.to/flwidmer/comment/18pf5

parseHead :: [String] -> [(String, [(String, Int)])]
parseHead [] = []
parseHead (x:xs) = (key,value):parseHead xs
  where keyValue = splitOn " contain " x
        key = concat . take 2 . words $ head keyValue
        value = map parseChilds $ splitOn "," $ keyValue !! 1 

parseChilds :: String -> (String, Int)
parseChilds "no other bags." = ("none", 0)
parseChilds x = (concat $ tail bags, read $ head bags)
  where rx = filter (/= '.') x
        bags = filter (\x -> x /="bags" && x /= "bag") $ words rx


iMap :: [(String, [(String,b)])] -> [(String,String)]
iMap = concatMap inv
  where inv (o, i) = map ((\x -> (x, o)) . fst) i

solve :: [(String,String)] -> Int
solve i = length $ nub $ recurse i "shinygold"

recurse :: [(String,String)] -> String -> [String]
recurse i s = cur ++ n
  where cur = look i s
        n = concatMap (recurse i) cur

look :: [(String, b)] -> String -> [b]
look i key = map snd $ filter (\(k,_) -> k == key) i

main = do
  input <- readFile "./input.txt"
  let bags = lines input
  putStr . show . solve . iMap $ parseHead bags
