import Test.HUnit

-- Problem 19
-- Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate [x] _ = [x]
rotate (x:xs) i
  | (abs i) > len = rotate (x:xs) (mod i len)
  | i == 0 = (x:xs)
  | otherwise  = rotate (xs ++ [x]) (i-1)
  where
    len = length (x:xs)
