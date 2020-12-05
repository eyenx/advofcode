import Data.List
import Data.List.Split 
import Text.Regex.Posix 

transform :: String -> String
transform "" = ""
transform x = intercalate " " $ sort $ splitOn " " x


validHeight :: String -> Bool
validHeight "" = False
validHeight x
  | x =~ "[0-9]{3}cm" && cm >= 150 && cm <= 193 = True
  | x =~ "[0-9]{2}in" && inch >= 59 && inch <= 76 = True
  | otherwise = False
  where cm = read $ splitOn "c" x !! 0 :: Int
        inch = read $ splitOn "i" x !! 0 :: Int

isValidField :: String -> Bool
isValidField "" = False
isValidField x
  | key == "byr" && intvalue >= 1920 && intvalue <= 2002  = True
  | key == "iyr" && intvalue >= 2010 && intvalue <= 2020  = True
  | key == "eyr" && intvalue >= 2020 && intvalue <= 2030  = True
  | key == "hgt" = validHeight strvalue
  | key == "hcl" = strvalue =~ "#[0-9a-f]{6}$"
  | key == "ecl" = elem strvalue ["amb","blu","brn","gry","grn","hzl","oth"]
  | key == "pid" = strvalue =~ "^[0-9]{9}$"
  | key == "cid" = True
  | otherwise = False
  where key = splitOn ":" x !! 0
        intvalue = read $ splitOn ":" x !! 1 :: Int
        strvalue = splitOn ":" x !! 1

isValidPassport :: String -> Bool
isValidPassport "" = False
isValidPassport x = filtered =~ "byr:.* ecl:.* eyr:.* hcl:.* hgt:.* iyr:.* pid:.*"
  where filtered = intercalate " " $ filter isValidField $ splitOn " " x 

main = do
  input <- readFile "./input.txt"
  let passports = map (transform . intercalate " ") .  filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines $ input
  putStr $ show $ length $ filter isValidPassport passports
