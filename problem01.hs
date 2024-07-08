import Test.HUnit

-- Problem 1
-- Find the first element of a list

mylast :: [a] -> Maybe a
mylast []       = Nothing
mylast [x]      = Just x
mylast (x : xs) = mylast xs

testMyLast :: Test
testMyLast = TestList [
    TestCase (assertEqual "Último de [1,2,3,4]" (Just 4) (mylast [1,2,3,4])),
    TestCase (assertEqual "Último de ['x','y','z']" (Just 'z') (mylast ['x','y','z'])),
    TestCase (assertEqual "Último de ['z']" (Just 'z') (mylast ['z'])),
    TestCase (assertEqual "Último de []" (Nothing :: Maybe Int) (mylast []))
    ]

main :: IO Counts
main = runTestTT testMyLast
