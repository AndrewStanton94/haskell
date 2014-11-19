absolute :: Int -> Int
absolute x
    | x >= 0    = x
    | otherwise = -x

sign :: Int -> Int
sign x
    | x > 0     = 1
    | x < 0     = -1
    |otherwise  = x

howManyEqual :: Int -> Int -> Int -> Int 
howManyEqual x y z
    | x == y && y == z              = 3
    | x == y || x == z || y == z    = 2
    | otherwise                     = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float 
sumDiagonalLengths x y z = diag x + diag y + diag z
    where
        diag a = sqrt(2 * a ^ 2)

taxiFare :: Int -> Float
taxiFare d
    | d <= 10   = fromIntegral d * 0.5 + 2.5
    | otherwise = (fromIntegral d - 10) * 0.3 + 7.5

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = aboveAverage x y z + aboveAverage y z x + aboveAverage z x y
    where
        aboveAverage a b c
            | fromIntegral a > average a b c    = 1
            | otherwise                         = 0

        average a b c = fromIntegral(a + b + c) / 3

validDate :: Int -> Int -> Bool
validDate d m
    | m == 4 || m == 5 || m == 9 || m == 11     = 0 < d && d <= 30
    | m == 2                                    = 0 < d && d <=28
    | otherwise                                 = 0 < d && d <=31

daysInMonth :: Int -> Int -> Int
daysInMonth m y
    | m == 4 || m == 5 || m == 9 || m == 11     = 30
    | m == 2                                    = if y `mod` 4 == 0 then 29 else 28
    | otherwise                                 = 31
