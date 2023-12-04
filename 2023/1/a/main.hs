import Data.Char (isDigit)

-- ExtractDigits from String
extractDigits :: String -> (Int, Int)
extractDigits str = (firstDigit, secondDigit)
  where
    digits = filter isDigit str
    firstDigit = read [head digits]
    secondDigit = read [last digits]

-- Function to extract the calibration values from a line
calibrationValue :: String -> Int
calibrationValue line = (10 * fst tDigits) + snd tDigits
  where
    tDigits = extractDigits line

-- Function to process a list of lines and calculate the sum of calibration values
totalCalibrationSum :: [String] -> Int
totalCalibrationSum = sum . map calibrationValue

main :: IO ()
main = do
  -- Read the lines from the input file
  input <- lines <$> readFile "./input"
  
  -- Calculate the total sum of calibration values
  let sumValues = totalCalibrationSum input

  -- Print the result
  putStrLn $ "Total sum of calibration values: " ++ show sumValues

