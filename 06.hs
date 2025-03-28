{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

alwaysEven :: (Int -> Int) -> [Int] -> Bool
-- alwaysEven f xs = length (filter even (map f xs)) == length xs
-- alwaysEven f xs = andAll (map (even . f) xs)
alwaysEven f = andAll . map (even . f)

updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
updatePositivesOnly f = map (\x -> if x > 0 then f x else x)

-- 1
mult10 :: [Int] -> [Int]
mult10 = map (* 10)

-- 2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- 3
orAll :: [Bool] -> Bool
orAll xs = length (filter id xs) == length xs
-- orAll = any id

-- 4
sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

-- 5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>=0) . filter (<= 10)

-- 6
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>= 0)

-- 7
countBetween :: Float -> Float -> [Float] -> Int
countBetween a b = length . filter (\x -> x >= a && x <= b) 

-- 8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f xs = length (filter (>=0) (map f xs)) == length xs
-- alwaysPositive f xs = andAll (map ((>=0) . f) xs)
alwaysPositive f = andAll . map ((>=0) . f)

-- 9
productSquareRoots :: [Float] -> Float
productSquareRoots = product . squareRoots

-- 10
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (e:xs) = if f e then xs else e : removeFirst f xs

-- 11
removeLast ::(a -> Bool) -> [a] -> [a]
removeLast f [] = []
removeLast f xs = reverse (removeFirst f (reverse xs))

-- 12
zeroToTenWithLambda :: [Int] -> [Int]
zeroToTenWithLambda = filter (\x -> x >= 0 && x <= 10)

-- 13
--- a
alwaysPositiveWithFoldr :: (Float -> Float) -> [Float] -> Bool
alwaysPositiveWithFoldr f = foldr (\x xs -> (f x >= 0) && xs) True

--- b
productSquareRootsWithFoldr :: [Float] -> Float
productSquareRootsWithFoldr = foldr (*) 1 . squareRoots

--- c
reverseWithFoldr :: [a] -> [a]
reverseWithFoldr = foldr (\x xs -> xs ++ [x]) []
