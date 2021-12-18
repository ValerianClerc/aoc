module Day3 where

sampleInput :: [String]
sampleInput =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

split :: Char -> String -> [Char] -> [String]
split sep (x : xs) acc =
  if sep == x then acc : split sep xs [] else split sep xs $ acc ++ [x]
split _ [] acc = [ acc | acc /= "" ]

-- TODO: rewrite to be more functional
solve :: [String] -> String
solve input = go input (0, 0, 0, 0, 0)
 where
  inputLen = length input
  getMostCommonBit x = if x > div inputLen 2 then "1" else "0"
  go (x : xs) (b0, b1, b2, b3, b4) = go
    xs
    ( b0 + (read [x !! 0] :: Int)
    , b1 + (read [x !! 1] :: Int)
    , b2 + (read [x !! 2] :: Int)
    , b3 + (read [x !! 3] :: Int)
    , b4 + (read [x !! 4] :: Int)
    )
  go [] (b0, b1, b2, b3, b4) =
    getMostCommonBit b0
      ++ getMostCommonBit b1
      ++ getMostCommonBit b2
      ++ getMostCommonBit b3
      ++ getMostCommonBit b4

invert :: String -> String
invert ('0' : xs) = '1' : invert xs
invert ('1' : xs) = '0' : invert xs
invert []         = []
invert n          = error $ "Invalid value in binary number: " ++ n

main :: IO ()
main = do
  fileContent <- readFile "./day3.txt"
  let parsedInput = split '\n' fileContent []
  let gamma       = solve parsedInput
  let epsilon     = invert gamma
  print $ "Gamma: " ++ gamma
  print $ "Epsilon: " ++ epsilon
  print
    "To be completed when I have wifi to research Binary manipulation in Haskell"
    -- TODO: need to multiply gamma and epsilon together
