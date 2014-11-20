import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

grade :: StudentMark -> Char
grade (_, x)
    | x < 40    = 'F'
    | x < 50    = 'D'
    | x < 60    = 'C'
    | x < 70    = 'B'
    | otherwise = 'A'

capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark > 40 = (name, 40)
    | otherwise = (name, mark)

firstNumbers :: Int -> [Int]
firstNumbers lim
    | lim < 1   = []
    | otherwise = [1 .. lim]

firstSquares :: Int -> [Int]
firstSquares lim = [i * i | i <- firstNumbers lim]

capitalise :: String -> String
capitalise str = [toUpper ch | ch <- str]

onlyDigits :: String -> String
onlyDigits strIn = [strOut | strOut <- strIn, isDigit strOut]

capMarks :: [StudentMark] -> [StudentMark]
capMarks lst = [capMark student | student <- lst]

markStudents :: [StudentMark] -> [(String, Char)]
markStudents lst = [(student, grade (student, mark)) | (student, mark) <- lst]

duplicate :: String -> Int -> String
duplicate str lim
    | lim == 0  = ""
    | otherwise = str ++ duplicate str (lim - 1)

divisors :: Int -> [Int]
divisors value = [factor | factor <- firstNumbers value, value `mod` factor == 0]

isPrime :: Int -> Bool
isPrime x
    | length (divisors x) == 2  = True
    | otherwise                 = False -- 1 ! prime

split :: [(c,d)] -> ([c], [d])
split lst = ([n | (n, m) <- lst], [m |(n,m) <- lst])