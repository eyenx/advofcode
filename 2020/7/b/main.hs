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

solve :: [(String, [(String,Int)])] -> Int
solve i = recurse i "shinygold"

recurse :: [(String, [(String,Int)])] -> String -> Int
recurse i s = sum (map snd cur) + n
  where cur = concat $ look i s
        n = sum $ map rv cur
        rv (b,m) = m * recurse i b

look :: [(String, b)] -> String -> [b]
look i key = map snd $ filter (\(k,_) -> k == key) i

main = do
  input <- readFile "./input.txt"
  let bags = lines input
  putStr . show . solve $ parseHead bags
