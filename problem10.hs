import Test.HUnit

-- Problem 10
-- Use problem 9 to implement run-length encoding

pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = pack' x [] [] xs
  where
    pack' x res ac [] = res ++ ([x:ac])
    pack' x res ac (y:ys)
      | x == y    = pack' x res (x:ac) ys
      | otherwise = pack' y (res++[x:ac]) [] ys

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\ys -> (length ys, head ys)) (pack xs)

testEncode :: Test
testEncode = TestList [
    TestCase (assertEqual "Encode []" ([] :: [(Int, Int)])  (encode [])),
    TestCase (assertEqual "Encode [1]" ([(1,1)])  (encode [1])),
    TestCase (assertEqual "Encode [1,2,3]" ([(1,1),(1,2),(1,3)])  (encode [1,2,3])),
    TestCase (assertEqual "Encode [1,1,1,2,3,3,4,5,5,5,5]" ([(3,1),(1,2),(2,3),(1,4),(4,5)])  (encode [1,1,1,2,3,3,4,5,5,5,5]))
    ]

main :: IO Counts
main = runTestTT testEncode
