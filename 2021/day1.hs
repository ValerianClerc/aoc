module Day1 where

-- was on a plane and couldn't search the docs
-- for a stdlib "split" function, so I wrote my own
split :: Char -> [Char] -> [Char] -> [Int]
split sep (x : xs) acc =
  if sep == x then read acc : split sep xs [] else split sep xs $ acc ++ [x]
split _ [] acc = [ read acc | acc /= "" ]

solve :: [Int] -> IO ()
solve input = print $ go 0 (head input) (tail input)
 where
  go :: Int -> Int -> [Int] -> Int
  go incrCount prev (x : xs) =
    if prev < x then go (incrCount + 1) x xs else go incrCount x xs
  go incrCount _ [] = incrCount

main :: IO ()
main = do
  fileContent <- readFile "./day1.txt"
  print $ split '\n' fileContent ""
  solve $ split '\n' fileContent ""
