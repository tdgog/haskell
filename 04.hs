import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
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

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

sumNumbersBetween :: Int -> Int -> Int
sumNumbersBetween x y = sum [x .. y]
-- sumNumbersBetween x y
-- | x > y = 0
-- | otherwise = x + sumNumbersBetween (x + 1) y

sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], even i]
-- sumEvenNumbersBetween x y
-- | x > y = 0
-- | mod x 2 == 0 = x + sumEvenNumbersBetween (x + 2) y
-- | otherwise = sumEvenNumbersBetween (x + 1) y

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
  where
    sumMarks = sum [mk | (_ , mk) <- stmks]
    numberOfStudents = length stmks

-- 1
sumDifference :: Int -> Int -> (Int,Int)
sumDifference a b = (a + b, a - b)

-- 2
grade :: StudentMark -> Char
grade (_, mk)
  | mk > 100 || mk < 0 = error "Grade out of range"
  | mk >= 70 = 'A'
  | mk >= 60 = 'B'
  | mk >= 50 = 'C'
  | mk >= 40 = 'D'
  | otherwise = 'F'
  
-- 3 
capMark :: StudentMark -> StudentMark
capMark (name, mk)
  | mk > 100 || mk < 0 = error "Grade out of range"
  | otherwise = (name, min mk 40)

-- 4
firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

-- 5
firstSquares :: Int -> [Int]
firstSquares n = [i ^ 2 | i <- firstNumbers n]

-- 6
capitalise :: String -> String
capitalise str = [toUpper c | c <- str]

-- 7
onlyDigits :: String -> String
onlyDigits str = [c | c <- str, isDigit c]

-- 8
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [capMark stmk | stmk <- stmks]

-- 9
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(name, grade (name, mk)) | (name, mk) <- stmks]

-- 10
duplicate :: String -> Int -> String
-- duplicate str times
--   | times == 1 = str
--   | otherwise = str ++ duplicate str (times - 1)
duplicate str times = concat [str | _ <- [1 .. times]]

-- 11
divisors :: Int -> [Int]
divisors n = [i | i <- [1 .. n], n `mod` i == 0]

-- 12
isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

-- 13
split :: [(a, b)] -> ([a], [b])
-- split x = (map fst x, map snd x)
split x = ([a | (a, _) <- x], [b | (_, b) <- x])
