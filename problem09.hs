import Test.HUnit

-- Problem 9
-- Pack consecutive duplicates of lists elements into sublists

pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = pack' x [] [] xs
  where
    pack' x res ac [] = res ++ ([x:ac])
    pack' x res ac (y:ys)
      | x == y    = pack' x res (x:ac) ys
      | otherwise = pack' y (res++[x:ac]) [] ys

testPack :: Test
testPack = TestList [
    TestCase (assertEqual "Pack []" ([[]] :: [[Int]])  (pack [])),
    TestCase (assertEqual "Pack [1]" ([[1]])  (pack [1])),
    TestCase (assertEqual "Pack [1,2,3]" ([[1],[2],[3]])  (pack [1,2,3])),
    TestCase (assertEqual "Pack [1,1,1,2,3,3,4,5,5,5,5]" ([[1,1,1],[2],[3,3],[4],[5,5,5,5]])  (pack [1,1,1,2,3,3,4,5,5,5,5]))
    ]

main :: IO Counts
main = runTestTT testPack
