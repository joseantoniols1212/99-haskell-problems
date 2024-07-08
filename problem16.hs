import Test.HUnit

-- Problem 16
-- Drop every N'th element from a list. 


dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs c = dropEvery' xs c c
  where
    dropEvery' [] _ _ = []
    dropEvery' (x:xs) c fix
      | c == 1 = dropEvery' xs fix fix
      | c > 1 = x:(dropEvery' xs (c-1) fix)
      | c < 1 = error "The drop interval must be a positive integer."

testDropEvery :: Test
testDropEvery = TestList [
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 1" ([])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 1)),
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 2" ([1,3,5,7,9,11])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 2)),
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 3" ([1,2,4,5,7,8,10,11])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 3)),
    TestCase (assertEqual "dropEvery abcdefghik 3" ("abdeghk")  (dropEvery "abcdefghik" 3)),
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 4" ([1,2,3,5,6,7,9,10,11])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 4)),
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 5" ([1,2,3,4,6,7,8,9,11,12])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 5)),
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 6" ([1,2,3,4,5,7,8,9,10,11])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 6)),
    TestCase (assertEqual "dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 7" ([1,2,3,4,5,6,8,9,10,11,12])  (dropEvery [1,2,3,4,5,6,7,8,9,10,11,12] 7)),
    TestCase (assertEqual "dropEvery [] 3" ([] :: [Int])  (dropEvery [] 3))
    ]

main :: IO Counts
main = runTestTT testDropEvery
