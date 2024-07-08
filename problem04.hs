import Test.HUnit

-- Problem 4
-- Find number of elements of a list

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

testMyLength :: Test
testMyLength = TestList [
    TestCase (assertEqual "Length of [1,2,3,4]" (4) (myLength [1,2,3,4])),
    TestCase (assertEqual "Length of [1]" (1) (myLength [1])),
    TestCase (assertEqual "Length of ['z']" (1) (myLength ['z'])),
    TestCase (assertEqual "Length of []" (0) (myLength []))
    ]

main :: IO Counts
main = runTestTT testMyLength
