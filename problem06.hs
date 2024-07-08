import Test.HUnit

-- Problem 6
-- Find out if a list is a palindrome

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = foldl (\x y -> x && (f y)) True  $ zip xs (reverse xs)
  where f (x, y) = x == y

testIsPalindrome :: Test
testIsPalindrome = TestList [
    TestCase (assertEqual "Is Palindrome [1,2,3,4]?" (False) (isPalindrome [4,3,2,1])),
    TestCase (assertEqual "Is Palindrome [4,3,3,4]?" (True) (isPalindrome [4,3,3,4])),
    TestCase (assertEqual "Is Palindrome [4,3,3,4]?" (True) (isPalindrome [4,3,4])),
    TestCase (assertEqual "Is Palindrome [1]?" (True) (isPalindrome [1])),
    TestCase (assertEqual "Is Palindrome ['z']?" (True) (isPalindrome ['z']))
    ]

main :: IO Counts
main = runTestTT testIsPalindrome
