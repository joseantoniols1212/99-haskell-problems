import Test.HUnit

-- Problem 12
-- Decode the encoding of problem 11

pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = pack' x [] [] xs
  where
    pack' x res ac [] = res ++ ([x:ac])
    pack' x res ac (y:ys)
      | x == y    = pack' x res (x:ac) ys
      | otherwise = pack' y (res++[x:ac]) [] ys

data EncodingValue a = Single a | Multiple Int a
  deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [EncodingValue a]
encodeModified [] = []
encodeModified xs = map f $ pack xs
  where
    f [y] = Single y
    f ys = Multiple (length ys) (head ys)

decodeModified :: [EncodingValue a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : (decodeModified xs)
decodeModified ((Multiple c x):xs) = (replicate c x) ++ (decodeModified xs)

testDecodeModified :: Test
testDecodeModified = TestList [
    TestCase (assertEqual "encodeModified []" ([] :: [EncodingValue Int])  (decodeModified [])),
    TestCase (assertEqual "encodeModified [1]" [1] (decodeModified [Single 1])),
    TestCase (assertEqual "encodeModified [1,2,3]" [1,2,3] (decodeModified [Single 1, Single 2, Single 3])),
    TestCase (assertEqual "encodeModified [1,1,1,2,3,3,4,5,5,5,5]" [1,1,1,2,3,3,4,5,5,5,5] (decodeModified [Multiple 3 1, Single 2, Multiple 2 3, Single 4, Multiple 4 5]))
    ]

main :: IO Counts
main = runTestTT testDecodeModified
