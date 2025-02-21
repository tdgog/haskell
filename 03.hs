-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&))
import GHC.Num (Integer(IN))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||
infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

fibonacci :: Int -> Int
-- fibonacci n
-- | n == 0 = 0
-- | n == 1 = 1
-- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Exercises
-- 1
-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- False && True = False
-- True && False = False
-- False && False = False

-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False

(&&) :: Bool -> Bool -> Bool
False && _     =  False
True && a    = a

-- 2
exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr False True = True
exOr True False = True
exOr False False = False

-- 3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse False _ b = b

-- 4
daysInMonth :: Int -> Int
daysInMonth 1 = 31
daysInMonth 2 = 28
daysInMonth 3 = 31
daysInMonth 4 = 30
daysInMonth 5 = 31
daysInMonth 6 = 30
daysInMonth 7 = 31
daysInMonth 8 = 31
daysInMonth 9 = 30
daysInMonth 10 = 31
daysInMonth 11 = 30
daysInMonth 12 = 31

validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month

-- 5
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- 6
sumSquares :: Int -> Int
sumSquares 1 = 1
sumSquares n = n^2 + sumSquares (n - 1) 

-- 7
power :: Int -> Int -> Int
power _ 0 = 1
power n p = n * (power n (p - 1))

-- 8
sumFromTo :: Int -> Int -> Int
sumFromTo n max
 | n > max = 0
 | otherwise = n + sumFromTo (n + 1) max

-- 9
