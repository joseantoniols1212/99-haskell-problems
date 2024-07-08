import Test.HUnit

-- Problem 13
-- Direct run-length encoding

data EncodingValue a = Single a | Multiple Int a
  deriving (Show, Eq)

encodeDirect :: Eq a => [a] -> [EncodingValue a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:y:ys)
  | x == y = encodeDirect' 2 y ys
  | otherwise = (Single x) : (encodeDirect (y:ys))
  where
    encodeDirect' a y [] = [Multiple a y]
    encodeDirect' a y (z:zs)
      | y == z = encodeDirect' (a+1) z zs
      | otherwise = (Multiple a y) : encodeDirect (z:zs)

testEncodeDirect :: Test
testEncodeDirect = TestList [
    TestCase (assertEqual "encodeDirect []" ([] :: [EncodingValue Int])  (encodeDirect [])),
    TestCase (assertEqual "encodeDirect [1]" ([Single 1])  (encodeDirect [1])),
    TestCase (assertEqual "encodeDirect [1,2,3]" ([Single 1, Single 2, Single 3])  (encodeDirect [1,2,3])),
    TestCase (assertEqual "encodeDirect [1,1,1,2,3,3,4,5,5,5,5]" ([Multiple 3 1, Single 2, Multiple 2 3, Single 4, Multiple 4 5]) (encodeDirect [1,1,1,2,3,3,4,5,5,5,5]))
    ]

main :: IO Counts
main = runTestTT testEncodeDirect
