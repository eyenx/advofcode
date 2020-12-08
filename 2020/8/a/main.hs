import Data.List
import Data.List.Split
import Text.Regex.Posix


bootCode :: [Int] -> Int -> Int ->  [String] -> Int
bootCode [] 0 0 [] = 0
bootCode ps p c [] = c
bootCode ps p c xs
  | elem p ps = c
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


main = do
  input <- readFile "./input.txt"
  let instructions = lines input
  putStr . show $ bootCode [] 0 0 instructions
