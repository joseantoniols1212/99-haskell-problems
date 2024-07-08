import Test.HUnit

-- Problem 8
-- Eliminate consecutive duplicates of list elements

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = compress' x xs
  where
    compress' x [] = [x] 
    compress' x (y:ys)
      | x == y    = compress' x ys
      | otherwise = x : (compress' y ys)

testCompress :: Test
testCompress = TestList [
    TestCase (assertEqual "Compress []" ([] :: [Int])  (compress [])),
    TestCase (assertEqual "Compress [1]" ([1])  (compress [1])),
    TestCase (assertEqual "Compress [1,2,3]" ([1,2,3])  (compress [1,2,3])),
    TestCase (assertEqual "Compress [1,1,1,2,3,3,4,5,5,5,5]" ([1,2,3,4,5])  (compress [1,1,1,2,3,3,4,5,5,5,5]))
    ]

main :: IO Counts
main = runTestTT testCompress
