import Data.List
import Data.List.Split 
import Text.Regex.Posix 

transform :: String -> String
transform "" = ""
transform x = intercalate " " $ sort $ splitOn " " x

main = do
  input <- readFile "./input.txt"
  let passports = map (intercalate " ") .  filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines $ input
  putStr $ show $ length $ filter (\x -> x =~ "byr:.*ecl:.*eyr:.*hcl:.*hgt:.*iyr:.*pid:.*" :: Bool) $ map (transform) passports
