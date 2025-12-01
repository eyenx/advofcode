
-- Define a data type to represent a single rotation
data Rotation = Rotation Char Int deriving (Show)

-- Function to parse a rotation string (e.g., "L68")
parseRotation :: String -> Rotation
parseRotation s = Rotation dir dist
  where
    dir  = head s
    dist = read (tail s)

-- The dial operates modulo 100
moduloValue :: Int
moduloValue = 100

-- Function to calculate the new dial position after a rotation
updateDial :: Int -> Rotation -> Int
updateDial currentPos (Rotation dir dist) =
  case dir of
    'R' -> (currentPos + dist) `mod` moduloValue
    'L' -> (currentPos - dist + moduloValue) `mod` moduloValue -- Add moduloValue to ensure the intermediate result is non-negative before the final mod
    _   -> error "Invalid rotation direction"

-- Function to process the list of rotations and track the dial positions
simulateRotations :: [Rotation] -> [Int]
simulateRotations rotations = scanl updateDial 50 rotations

solve :: String -> Int
solve input =
  let
    -- 1. Parse the input lines into a list of Rotation data types
    rotations = map parseRotation $ lines input

    -- 2. Simulate the rotations to get a list of all dial positions (including the start)
    allPositions = simulateRotations rotations

    -- 3. We are interested in the positions *after* each rotation, so we drop the starting position (50)
    finalPositions = tail allPositions

    -- 4. Count how many of these final positions are 0
    zeroCount = length (filter (== 0) finalPositions)
  in
    zeroCount

main :: IO ()
main = do
  -- Read the lines from the input file
  input <- lines <$> readFile "./input"
  
  -- Solve it
  let solution = solve $ unlines input

  -- Print the result
  putStrLn $ "Total sum of 0 values: " ++ show solution
