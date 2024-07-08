import Test.HUnit

-- Problem 7
-- Flatten a nested list structure

data NestedList a = Elem a | List [NestedList a]
  deriving (Show)

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List ((Elem x):xs)) = x : flatten (List xs)
flatten (List (x: xs)) = (flatten x) ++ flatten (List xs)


testFlatten :: Test
testFlatten = TestList [
    TestCase (assertEqual "Flatten List []" ([] :: [Int])  (flatten (List []))),
    TestCase (assertEqual "Flatten Elem 1" ([1])  (flatten (Elem 1))),
    TestCase (assertEqual "Flatten List [Elem 1, List [Elem 2, Elem 3]]" ([1,2,3])  (flatten (List [Elem 1, List [Elem 2, Elem 3]]))),
    TestCase (assertEqual "Flatten List [List [Elem 1, Elem 4], List [Elem 2, Elem 3]]" ([1,4,2,3])  (flatten (List [List [Elem 1, Elem 4], List [Elem 2, Elem 3]]))),
    TestCase (assertEqual "Flatten List [Elem 1, Elem 2]" ([1,2])  (flatten (List [Elem 1, Elem 2])))
    ]

main :: IO Counts
main = runTestTT testFlatten
