import Test.HUnit

-- Problem 5
-- Reverse list

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

testMyReverse :: Test
testMyReverse = TestList [
    TestCase (assertEqual "Reverse of [1,2,3,4]" ([1,2,3,4]) (myReverse [4,3,2,1])),
    TestCase (assertEqual "Reverse of [1]" ([1]) (myReverse [1])),
    TestCase (assertEqual "Reverse of ['z']" (['z']) (myReverse ['z'])),
    TestCase (assertEqual "Reverse of []" ([] :: [Int]) (myReverse []))
    ]

main :: IO Counts
main = runTestTT testMyReverse
