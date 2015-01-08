--languageUsed :: String -> String
languageUsed "MATHFUN" = "Haskell"
languageUsed "ADPROC" = "Java"
languageUsed _ = "None"

testData1 = ["ADPROC", "MATHFUN", "DSALG", "MATHFUN"]

-- unitsToLanguages :: [String] -> [String]
unitsToLanguages [] = []
unitsToLanguages (x:xs) = languageUsed x : unitsToLanguages xs


testData2 = [("John", "ADPROC"), ("Jo", "MATHFUN"), ("Jim", "ADPROC")]
programsIn :: [(String, String)] -> [(String, String)]
programsIn list = [(name, languageUsed subject) | (name, subject) <- list]


tenths :: [Float] -> [Float]
tenths = map (/10)
