addWord :: String -> [String] -> [String]
addWord strIn listIn =  listIn ++ [strIn]

wordsToString :: [String] -> String
wordsToString [] = ""
wordsToString (x:xs) = x ++ "\n" ++ wordsToString xs

wordsOfLength :: Int -> [String] -> [String]
-- wordsOfLength givenLength listIn = filter (givenLength == length ) listIn 
wordsOfLength givenLength listIn = [word | word <- listIn, length word == givenLength]

--main :: IO ()
main = do
    content <- readFile "words.txt"    -- This is a string
    let listData = read content :: [String]     -- info = ([String]) content
    let listData2 = addWord "Lemon" listData    -- Appended []
    let strData = wordsToString (listData2)     -- (String)
    putStrLn (strData)
    writeFile "words.txt" (show listData2)

-- return: cast to IO
-- read String :: type: cast
-- show var: cast to String

--menu = do
	


--	h(f(g(a)))
--	=
--	h $ f $ g a

-- http://learnhaskell.blogspot.co.uk/2007/09/lesson-3-case-3.html
-- http://zvon.org/other/haskell/Outputsyntax/caseQexpressions_reference.html
-- http://stackoverflow.com/questions/940382/haskell-difference-between-dot-and-dollar-sign
-- http://blog.ezyang.com/2011/11/how-to-read-haskell/
