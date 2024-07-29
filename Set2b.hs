module Set2b where

-- Some imports you'll need. Don't add other imports :)
import Data.List
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial n k
  | k == 0 = 1
  | n == 0 = 0
  | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)

------------------------------------------------------------------------------
-- Ex 2: implement the odd factorial function. Odd factorial is like
-- factorial, but it only multiplies odd numbers.
--
-- Examples:
--   oddFactorial 7 ==> 7*5*3*1 ==> 105
--   oddFactorial 6 ==> 5*3*1 ==> 15

oddFactorial :: Integer -> Integer
oddFactorial n
  | n == 1 = 1
  | even n = oddFactorial (n - 1)
  | otherwise = n * oddFactorial (n - 2)

------------------------------------------------------------------------------
-- Ex 3: implement the Euclidean Algorithm for finding the greatest
-- common divisor:
--
-- Given two numbers, a and b,

-- * if one is zero, return the other number

-- * if not, subtract the smaller number from the larger one

-- * replace the larger number with this new number

-- * repeat

--
-- For example,
--   myGcd 9 12 ==> 3
-- In this case, the algorithm proceeds like this
--
--   a      b
--
--   9      12
--   9      (12-9)
--   9      3
--   (9-3)  3
--   6      3
--   (6-3)  3
--   3      3
--   (3-3)  3
--   0      3
--
-- Background reading:

-- * https://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd x y
  | x == 0 = y
  | y == 0 = x
  | x == y = x
  | x < y = myGcd x (y - x)
  | x > y = myGcd (x - y) y

------------------------------------------------------------------------------
-- Ex 4: Implement the function leftpad which adds space characters
-- to the start of the string until it is long enough.
--
-- Examples:
--   leftpad "foo" 5 ==> "  foo"
--   leftpad "13" 3 ==> " 13"
--   leftpad "xxxxx" 3 ==> "xxxxx"
--
-- Tips:

-- * you can combine strings with the ++ operator.

-- * you can compute the length of a string with the length function

leftpad :: String -> Int -> String
leftpad xs x
  | length xs >= x = xs
  | otherwise = blanks (x - length xs) ++ xs

blanks :: Int -> String
blanks x = case x of
  1 -> " "
  x -> " " ++ blanks (x - 1)

------------------------------------------------------------------------------
-- Ex 5: let's make a countdown for a rocket! Given a number, you
-- should produce a string that says "Ready!", counts down from the
-- number, and then says "Liftoff!".
--
-- For example,
--   countdown 4 ==> "Ready! 4... 3... 2... 1... Liftoff!"
--
-- Hints:

-- * you can combine strings with the ++ operator

-- * you can use the show function to convert a number into a string

-- * you'll probably need a recursive helper function

countdown :: Integer -> String
countdown x = "Ready! " ++ countdownString x "... " ++ "Liftoff!"

countdownString :: Integer -> String -> String
countdownString x s
  | x == 1 = show x ++ s
  | otherwise = show x ++ s ++ countdownString (x - 1) s

------------------------------------------------------------------------------
-- Ex 6: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number evenly.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!
--
-- Hint: remember the mod function!

smallestDivisor :: Integer -> Integer
smallestDivisor x = smallestDivisorLoop x 2

smallestDivisorLoop :: Integer -> Integer -> Integer
smallestDivisorLoop x y
  | y == x = x
  | mod x y == 0 = y
  | otherwise = smallestDivisorLoop x (y + 1)

------------------------------------------------------------------------------
-- Ex 7: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime x
  | x == 0 = False
  | x == 1 = False
  | otherwise = x == smallestDivisor x

------------------------------------------------------------------------------
-- Ex 8: implement a function biggestPrimeAtMost that returns the
-- biggest prime number that is less than or equal to the given
-- number. Use the function isPrime you just defined.
--
-- You don't need to care about arguments less than 2. Any behaviour
-- for them is fine.
--
-- Examples:
--   biggestPrimeAtMost 3 ==> 3
--   biggestPrimeAtMost 10 ==> 7

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost x = if isPrime x then x else biggestPrimeAtMost (x - 1)