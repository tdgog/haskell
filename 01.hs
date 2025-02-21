-- 2
sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

-- 6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

-- 7
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = mod a b == 0

-- 8
isEven :: Int -> Bool
isEven a = divisibleBy a 2

-- 9
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) / 3

-- 10
absolute :: Int -> Int
absolute a = if a > 0 then a else (-a)
