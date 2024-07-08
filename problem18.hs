import Test.HUnit

-- Problem 18
-- Extract a slice from a list. 
-- Given two indices, i and j, the slice is the list containing the elements between the i'th and j'th
-- element of the original list (both limits included). Start counting the elements with 1. 

slice :: [a] -> Int -> Int -> Maybe [a]
slice [] _ _ = Nothing
slice (x:xs) i j
  | i > j             = Nothing
  | i < 1 || j < 1    = Nothing
  | i == 1 && j == 1  = Just [x]
  | i == 1 && j > 1   = do ys <- slice xs i (j-1)
                           return (x:ys)
  | i > 1             = slice xs (i-1) (j-1)

testslice :: Test
testslice = TestList [
    TestCase (assertEqual "slice [1,2,3,4] 2 3" (Just [2,3])  (slice [1,2,3,4] 2 3)),
    TestCase (assertEqual "slice [1,2,3,4] 3 3" (Just [3])  (slice [1,2,3,4] 3 3)),
    TestCase (assertEqual "slice [1,2,3,4] 3 4" (Just [3, 4])  (slice [1,2,3,4] 3 4)),
    TestCase (assertEqual "slice [1,2,3,4] 3 5" (Nothing)  (slice [1,2,3,4] 3 5)),
    TestCase (assertEqual "slice [1,2,3,4] 3 2" (Nothing)  (slice [1,2,3,4] 3 2)),
    TestCase (assertEqual "slice [1,2,3,4] 0 3" (Nothing)  (slice [1,2,3,4] 0 3)),
    TestCase (assertEqual "slice [] 1 3" (Nothing :: Maybe [Int])  (slice [] 1 3))
    ]

main :: IO Counts
main = runTestTT testslice
