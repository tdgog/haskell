{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head :: [p] -> p
head (x : _) = x
head [] = error "head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "tail: empty list"

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

-- For question 10
type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]


countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
  | x == ' ' = 1 + countSpaces xs
  | otherwise = countSpaces xs

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x <= y = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys


-- 1
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x : _) = x + 1

-- 2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x:x:xs

-- 3
rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (a : b : xs) = b:a:xs

-- 4
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- 5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

-- 6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

-- 7
orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs

-- 8
countIntegers :: Int -> [Int] -> Int
countIntegers a [] = 0
countIntegers a (x:xs)
  | a == x = 1 + countIntegers a xs
  | otherwise = countIntegers a xs

-- 9
removeAll :: Int -> [Int] -> [Int]
removeAll a [x]
  | a == x = []
  | otherwise = [x]
removeAll a (x:xs)
  | a == x = removeAll a xs
  | otherwise = x : removeAll a xs

-- 10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst a (x:xs)
    | a == x = x : removeAll a xs
    | otherwise = x : removeAllButFirst a xs

-- 11
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((student, mark):xs)
  | name == student = mark : listMarks name xs
  | otherwise = listMarks name xs

-- 12
sorted :: [Int] -> Bool
sorted [_] = True
sorted (a:b:xs)
  | a > b = False
  | otherwise = sorted (b:xs)

-- 13
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
  | x == y = prefix xs ys
  | otherwise = False

-- 14
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence (x:xs) (y:ys)
    | prefix (x:xs) (y:ys) = True
    | otherwise = subSequence (x:xs) ys
    