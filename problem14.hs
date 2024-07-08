import Test.HUnit

-- Problem 14
-- Duplicate elements of a list

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

testDupli :: Test
testDupli = TestList [
    TestCase (assertEqual "dupli []" ([] :: [Int])  (dupli [])),
    TestCase (assertEqual "dupli [1]" ([1, 1])  (dupli [1])),
    TestCase (assertEqual "dupli [1,2,3]" ([1, 1, 2, 2, 3, 3])  (dupli [1,2,3]))
    ]

main :: IO Counts
main = runTestTT testDupli
