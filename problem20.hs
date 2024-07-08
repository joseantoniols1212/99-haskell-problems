import Test.HUnit

-- Problem 20
-- Remove the K'th element from a list. 

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt i xs
  | 1 <= i && i <= len = remove' i xs []
  | otherwise         = Nothing
  where
    len = length xs
    remove' i (y:ys) ac
      | i == 1 = Just (y, ac ++ ys)
      | i > 1 = remove' (i-1) ys (ac++[y])
