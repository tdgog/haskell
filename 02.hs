heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 && bpm > 130 = "High heart rate for 61-80!"
    | age > 40 && bpm > 140 = "High heart rate for 41-60!"
    | age > 20 && bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
    where
        area = pi * (fromIntegral diameter / 2) ^ 2
        toppingCalories
            | toppings == "pepperoni" = 6
            | toppings == "tuna" = 4
            | toppings == "veggie" = 2.5
            | otherwise = 0

-- Exercises
-- 1
absolute :: Int -> Int
absolute a
    | a > 0 = a
    | otherwise = -a

-- 2
sign :: Int -> Int
sign a
    | a > 0 = 1
    | a == 0 = 0
    | otherwise = -1

-- 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | a == b && b == c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 0

-- 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = sum [diagonalLength a, diagonalLength b, diagonalLength c]
    where 
        diagonalLength :: Float -> Float
        diagonalLength sideLength = sqrt (2 * sideLength * sideLength)

-- 5
taxiFare :: Int -> Float
taxiFare d
    | d <= 10 = 2.20 + fromIntegral d * 0.5
    | otherwise = taxiFare 10 + (fromIntegral d - 10) * 0.30

-- 6
{-
Test cases:
    1 1 1 -> 0
    1 1 2 -> 1
    1 2 2 -> 2
-}
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = length (filter (> average) [a, b, c])
    where
        average = (a + b + c) `div` 3

-- 7
validDate :: Int -> Int -> Bool
validDate day month
    | elem month [1, 3, 5, 7, 8, 10, 12] && day <= 31 = True
    | elem month [4, 6, 9, 11] && day <= 30 = True
    | month == 2 && day <= 28 = True
    | otherwise = False

-- 8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month `elem` [1, 3, 5, 7, 8, 10, 11] = 31
    | month `elem` [4, 6, 9, 11] = 30
    | mod year 4 == 0 = 29
    | otherwise = 28

-- Written exercises
-- 1
{-
    sumThree 3 5 7
    -> 3 + 5 + 7                            def of sumThree
    -> 3 + 12                               arithmetic
    -> 15                                   arithmetic

    sumThree 8 (1 + 3) 2
    -> 8 + 4 + 2                            def of sumThree
    -> 8 + 6                                arithmetic
    -> 14                                   arithmetic
-}

-- 2
{-
    threeDifferent 1 4 2
    -> 1 /= 4 && 4 /= 2 && 1 /= 2           def of threeDifferent
    -> 1 /= 4 && 4 /= 2 && True             def of /=
    -> 1 /= 4 && True && True               def of /=
    -> 1 /= 4 && True                       def of &&
    -> True && True                         def of /=
    -> True                                 def of &&

    threeDifferent 1 7 7
    -> 1 /= 7 && 7 /= 7 && 1 /= 7           def of threeDifferent
    -> 1 /= 7 && 7 /= 7 && True             def of /=
    -> 1 /= 7 && False && True              def of /=
    -> 1 /= 7 && False                      def of &&
    -> True && False                        def of /=
    -> False                                def of &&
-}

-- 3
{-
    howManyEqual 3 5 2                      
    ?? 3 == 5 && 5 == 2                     first guard
    ?? -> 3 == 5 && False                   def of ==
    ?? -> False && False                    def of ==
    ?? -> False                             def of &&
    ?? 3 == 5 || 5 == 2 || 3 == 2           second guard
    ?? -> 3 == 5 || 5 == 2 || False         def of ==
    ?? -> 3 == 5 || False || False          def of ==
    ?? -> False || False || False           def of ==
    ?? -> False || False                    def of ||
    ?? -> False                             def of ||
    ?? otherwise                            third guard
    ?? -> 0                                 def of otherwise
    -> 0                                    

    howManyEqual 5 2 5                   
    ?? 5 == 2 && 2 == 5                     first guard
    ?? -> 5 == 2 && False                   def of ==
    ?? -> False && False                    def of ==
    ?? -> False                             def of &&
    ?? 5 == 2 || 2 == 5 || 5 == 5           second guard
    ?? -> 5 == 2 || 2 == 5 || True          def of ==
    ?? -> 5 == 2 || False || True           def of ==
    ?? -> False || False || True            def of ==
    ?? -> False || True                     def of ||
    ?? -> True                              def of ||
    -> True                                    
-}
