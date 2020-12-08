import Data.List
import Data.List.Split
import Text.Regex.Posix


bootCode :: [Int] -> Int -> Int ->  [String] -> Int
bootCode [] 0 0 [] = 0
bootCode ps p c [] = c
bootCode ps p c xs
  | elem p ps = 0
  | p == length xs = c
  | x =~ "acc" :: Bool = bootCode (ps++[p]) (p+1) (calc c x) xs
  | x =~ "jmp" :: Bool = bootCode (ps++[p]) (calc p x) c xs
  | x =~ "nop" :: Bool = bootCode (ps++[p]) (p+1) c xs
  | otherwise = c
  where x = xs !! p

calc :: Int -> String -> Int 
calc 0 "" = 0
calc c "" = c
calc c x
  | x =~ "\\+" :: Bool = c + read (splitOn "+" x !! 1) :: Int
  | x =~ "\\-" :: Bool = c - read (splitOn "-" x !! 1) :: Int
  | otherwise = 0 

genLists :: Int -> String -> String -> [String] -> [[String]]
genLists p o n l
  | p == length l  = []
  | x =~ o = replaceItem p n l : genLists (p+1) o n l
  | otherwise = l : genLists (p+1) o n l
  where x = l !! p

replaceItem :: Int -> String ->  [String] -> [String]
replaceItem p n l = take p l ++ n : drop (p+1) l

main = do
 input <- readFile "./input.txt"
 let instructions = lines input
 putStr . show $ (filter (>0) $ map (bootCode [] 0 0) (genLists 0 "jmp" "nop" instructions ++ genLists 0 "nop" "jmp" instructions)) !! 0
