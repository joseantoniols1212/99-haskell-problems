import Test.HUnit

-- Problem 15
-- Replicate the elements of a list a given number of times


repli :: [a] -> Int -> [a]
repli _ c
  | c <= 0 = error "The given number of replications must be a positive integer"
repli [] _ = []
repli (x:xs) c = (f x c []) ++ (repli xs c)
  where
    f x c xs
      | c == 1 = x : xs
      | otherwise = f x (c-1) (x:xs)

testrepli :: Test
testrepli = TestList [
    TestCase (assertEqual "repli []" ([] :: [Int])  (repli [] 2)),
    TestCase (assertEqual "repli [1] 2" ([1, 1])  (repli [1] 2)),
    TestCase (assertEqual "repli [1,2,3] 2" ([1, 1, 2, 2, 3, 3])  (repli [1,2,3] 2)),
    TestCase (assertEqual "repli [1,2,3] 3" ([1, 1, 1, 2, 2, 2, 3, 3, 3])  (repli [1,2,3] 3))
    ]

main :: IO Counts
main = runTestTT testrepli
