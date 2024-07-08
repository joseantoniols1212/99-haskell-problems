import Test.HUnit

-- Problem 22
-- Create a list containing all integers within a given range.

range :: Int -> Int -> Maybe [Int]
range i j
  | j < i = Nothing
  | i == j = Just [i]
  | otherwise = do x <- (range (i+1) j); return (i:x)
