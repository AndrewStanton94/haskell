helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines

-- For exercise 6
fahrenheit2Celsius :: Float -> Float
fahrenheit2Celsius f = (f - 32) * 5 / 9
 
celsius2Fahrenheit :: Float -> Float
celsius2Fahrenheit c = c * 9 / 5 + 32

greeting :: IO ()
greeting = do
	putStrLn "What is your name"
	name <- getLine
	putStrLn ("Hello " ++ name)

addTwoNumbers :: IO ()
addTwoNumbers = do
	putStrLn "Enter first number"
	v1 <- getInt
	putStrLn "Enter second number"
	v2 <- getInt
	print (v1 + v2)

copyFile :: IO ()
copyFile = do
	putStrLn "File to copy"
	from <- getLine
	putStrLn "Store in"
	to <- getLine
	fileContent <- readFile from
	writeFile to fileContent

-- buildList :: [String] -> IO ()

-- listBuilder = buildList 
