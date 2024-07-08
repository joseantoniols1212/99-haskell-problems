import Test.HUnit

-- Problem 3
-- Find the k'th element of a list

elementAt :: [a] -> Int -> Maybe a
elementAt (x:_) 0  = Just x
elementAt (_:xs) i
  | i > 0     = elementAt xs (i-1)
elementAt _ _ = Nothing

testElementAt :: Test
testElementAt = TestList [
    TestCase (assertEqual "Element at 1 of [1,2,3,4]" (Just 2) (elementAt [1,2,3,4] 1)),
    TestCase (assertEqual "Element at 4 of [1,2,3,4]" (Nothing) (elementAt [1,2,3,4] 4)),
    TestCase (assertEqual "Element at -1 of [1,2,3,4]" (Nothing) (elementAt [1,2,3,4] (-1))),
    TestCase (assertEqual "Element at 0 of []" (Nothing :: Maybe Int) (elementAt [] 0))
    ]

main :: IO Counts
main = runTestTT testElementAt
