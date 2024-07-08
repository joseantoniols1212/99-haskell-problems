import Test.HUnit

-- Problem 2
-- Fund the last two elements of a list

myButLast :: [a] -> Maybe (a, a)
myButLast []        = Nothing
myButLast [x]       = Nothing
myButLast (y : [x]) = Just (y, x)
myButLast (x : xs)  = myButLast xs

testMyButLast :: Test
testMyButLast = TestList [
    TestCase (assertEqual "Último de [1,2,3,4]" (Just (3,4)) (myButLast [1,2,3,4])),
    TestCase (assertEqual "Último de ['y','z']" (Just ('y','z')) (myButLast ['x','y','z'])),
    TestCase (assertEqual "Último de ['z']" (Nothing) (myButLast ['z'])),
    TestCase (assertEqual "Último de []" (Nothing :: Maybe (Int,Int)) (myButLast []))
    ]

main :: IO Counts
main = runTestTT testMyButLast
