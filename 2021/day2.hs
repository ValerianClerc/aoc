module Day2 where

sampleInput :: [(String, Int)]
sampleInput =
  [ ("forward", 5)
  , ("down"   , 5)
  , ("forward", 8)
  , ("up"     , 3)
  , ("down"   , 8)
  , ("forward", 2)
  ]

split :: Char -> String -> [Char] -> [String]
split sep (x : xs) acc =
  if sep == x then acc : split sep xs [] else split sep xs $ acc ++ [x]
split _ [] acc = [ acc | acc /= "" ]

solve :: [(String, Int)] -> Int
solve input = go input 0 0
 where
  go (("forward", n) : xs) depth pos = go xs depth (pos + n)
  go (("down"   , n) : xs) depth pos = go xs (depth + n) pos
  go (("up"     , n) : xs) depth pos = go xs (depth - n) pos
  go []                    depth pos = depth * pos
  go _                     _     _   = error "Invalid input"

main :: IO ()
main = do
  fileContent <- readFile "./day2.txt"
  let parsedInput =
        map ((\(x, y) -> (x, read y :: Int)) . span (/= ' '))
          $ split '\n' fileContent []
  let solution = solve parsedInput
  print solution
