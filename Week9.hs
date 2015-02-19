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
    --putStrLn (show (addWord "Lemon" info))
    --putStrLn (show (length info))

-- return: cast to IO
-- read String :: type: cast
-- show var: cast to String