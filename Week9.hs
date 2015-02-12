addWord :: String -> [String] -> [String]
addWord strIn listIn =  listIn ++ [strIn]

wordsToString :: [String] -> String
wordsToString [] = ""
wordsToString (x:xs) = x ++ "\n" ++ wordsToString xs

wordsOfLength :: Int -> [String] -> [String]
-- wordsOfLength givenLength listIn = filter (givenLength == length ) listIn 
wordsOfLength givenLength listIn = [word | word <- listIn, length word == givenLength]