import Test.HUnit

-- Problem 17
-- Split a list into two parts; the length of the first part is given.


split :: [a] -> Int -> Maybe ([a], [a])
split xs c
  | (c <= length xs) && (c >= 0) = Just (split' xs c [])
  | otherwise                    = Nothing
  where
    split' xs 0 ac = (ac, xs)
    split' (x:xs) c ac
      | c == 1 = (ac ++ [x], xs)
      | c > 1  = split' xs (c-1) (ac ++ [x])

testSplit :: Test
testSplit = TestList [
    TestCase (assertEqual "split [1,2,3,4,5,6,7,8,9,10,11,12] 1" (Just ([1], [2,3,4,5,6,7,8,9,10,11,12]))  (split [1,2,3,4,5,6,7,8,9,10,11,12] 1)),
    TestCase (assertEqual "split [1,2,3,4,5,6,7,8,9,10,11,12] 2" (Just ([1,2], [3,4,5,6,7,8,9,10,11,12]))  (split [1,2,3,4,5,6,7,8,9,10,11,12] 2)),
    TestCase (assertEqual "split abcdefghik 3" (Just ("abc", "defghik"))  (split "abcdefghik" 3)),
    TestCase (assertEqual "split [] 0" ((Just ([], [])) :: Maybe ([Int],[Int]))  (split [] 0)),
    TestCase (assertEqual "split [] 1" (Nothing :: Maybe ([Int],[Int]))  (split [] 1)),
    TestCase (assertEqual "split abcdefghik 50" (Nothing)  (split "abcdefghik" 50)),
    TestCase (assertEqual "split abcdefghik (-1)" (Nothing)  (split "abcdefghik" (-1)))
    ]

main :: IO Counts
main = runTestTT testSplit
