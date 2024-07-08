import Test.HUnit

-- Problem 21
-- Insert an element at a given position into a list.

-- The given position will be the index (starting at 1) that
-- the inserted element will have.

insertAt :: a -> [a] -> Int -> Maybe [a]
insertAt x [] 1 = Just [x]
insertAt _ [] _ = Nothing
insertAt x xs i
  | 1 <= i && i <= len+1 = Just (insertAt' x xs i)
  | otherwise            = Nothing
  where
    len = length xs
    insertAt' x [] 1 = [x]
    insertAt' x (y:ys) 1 = x:y:ys
    insertAt' x (y:ys) i = y : (insertAt' x ys (i-1))
