import System.IO

main :: IO ()
main = do
  input <- readFile "input"
  let result = f $ map read $ lines input
  print result

-- | Gets all combinations of a certain size of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 []        = [[]]
combinations _ []        = []
combinations 0 _         = [[]]
combinations size (x:xs) = [x:ps | ps <- combinations (size - 1) xs] ++ combinations size xs

f :: [Int] -> Int
f = foldl1 (*) . head . filter (\xs -> 2020 == sum xs) . combinations 2
