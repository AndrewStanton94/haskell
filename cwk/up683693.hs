-- 
-- MATHFUN - Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2014/15
-- UP683693
--

-- Types
type Name = String
type Cast = [String]
type Year = Int
type Fans = [String]

-- Define Film type here 
data Film = Film Name Cast Year Fans
--String [String] Int [String]
           deriving (Show,Read)
			    --title cast year fans

-- My test data
aNewFilm = Film "Sherlock" ["Benedict", "Martin"] 2050 ["Molly", "Mycroft", "Gavin", "Graham", "Jim"]

testDatabase :: [Film]
testDatabase = [ Film "Casino Royale" ["Daniel Craig", "Eva Green", "Judi Dench"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"], Film "Cowboys & Aliens" ["Harrison Ford", "Daniel Craig", "Olivia Wilde"] 2011 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"], Film "Catch Me If You Can" ["Leonardo DiCaprio", "Tom Hanks"] 2002 ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"], Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan", "Colin Firth"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"], Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"], Film "Quantum of Solace" ["Daniel Craig", "Judi Dench"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"], Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"], Film "Collateral" ["Tom Cruise", "Jamie Foxx"] 2004 ["Dave", "Garry", "Megan", "Sam", "Wally"], Film "The Departed" ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"] 2006 ["Zoe", "Emma", "Paula", "Olga", "Dave"], Film "Up in the Air" ["George Clooney", "Vera Farmiga"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"], Film "Gravity" ["George Clooney", "Sandra Bullock"] 2013 ["Zoe", "Emma", "Garry", "Ian", "Neal", "Wally", "Olga", "Dave"], Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"], Film "Ocean's Twelve" ["George Clooney", "Matt Damon", "Catherine Zeta-Jones", "Julia Roberts"] 2004 ["Jo", "Wally", "Emma"], Film "The Adjustment Bureau" ["Matt Damon", "Emily Blunt"] 2011 ["Kevin", "Tim", "Emma", "Emma", "Garry", "Ian", "Neal"], Film "Cloud Atlas" ["Tom Hanks", "Halle Berry"] 2012 ["Dave", "Amy", "Garry", "Ian", "Neal"], Film "The Reader" ["Kate Winslet", "Ralph Fiennes"] 2008 ["Emma", "Bill", "Dave", "Liz"], Film "Begin Again" ["Keira Knightley", "Mark Ruffalo", "James Corden"] 2013 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma"], Film "Revolutionary Road" ["Leonardo DiCaprio", "Kate Winslet"] 2008 ["Wally", "Sam", "Dave", "Jo"], Film "Into the Woods" ["Meryl Streep", "Emily Blunt", "James Corden"] 2014 ["Dave", "Jo", "Wally", "Emma"], Film "Now You See Me" ["Jesse Eisenberg", "Mark Ruffalo"] 2013 ["Bill", "Sam", "Zoe", "Jo"], Film "Larry Crowne" ["Tom Hanks", "Julia Roberts"] 2011 ["Liz", "Wally"], Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"], Film "Edge of Tomorrow" ["Tom Cruise", "Emily Blunt"] 2014 ["Jo", "Chris", "Wally", "Ian", "Garry", "Bill", "Olga", "Megan", "Sam"], Film "Django Unchained" ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"] 2012 ["Kevin", "Tim", "Emma", "Olga"], Film "Skyfall" ["Daniel Craig", "Judi Dench", "Ralph Fiennes"] 2012 ["Bill", "Olga", "Zoe", "Paula", "Megan", "Sam", "Wally" ]]

-- 
--
--  Your functional code goes here
--
--
addFilm :: [Film] -> Film -> [Film]
addFilm existingList newFilm = existingList ++ [newFilm]
    -- appends. : Would prepend
    -- addFilm testDatabase aNewFilm


filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((Film name cast year fans) : films) = name ++ " (" ++ show year ++ ")\n" ++ showCast cast ++   showFans fans ++ filmsAsString films
    where
        showCast cast = "Starring: " ++ wordsToString cast ++ "\n"
            where
                wordsToString :: [String] -> String
                wordsToString [] = ""
                wordsToString (x:xs)
                    | length (x:xs) == 1    = x ++ "."
                    | otherwise             = x ++ ", " ++ wordsToString xs

        showFans fans = show (length fans) ++ " fans" ++ "\n\n"
    -- putStrLn $ filmsAsString testDatabase

isFanOf :: [Film] -> String -> [Film]
isFanOf films fan = [(Film name cast year fans) | (Film name cast year fans) <- films, elem fan fans]
    -- putStrLn $ filmsAsString  $ isFanOf testDatabase "Olga"

filmsInPeriod :: [Film] -> Int -> Int -> [Film]
filmsInPeriod films min max =  [(Film name cast year fans) | (Film name cast year fans) <- films, min <= year && year <= max]
    -- putStrLn $ filmsAsString  $ filmsInPeriod testDatabase 2010 2015

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1   = putStrLn all films after adding 2014 film "The Monuments Men" 
--                   starring "George Clooney", "Matt Damon" and "Bill Murray" 
--                   to testDatabase
--demo 2   = putStrLn (filmsAsString testDatabase)
--demo 3   = putStrLn all films that Zoe is a fan of
--demo 4   = putStrLn all fans of Titanic
--demo 5   = putStrLn all films between 2010 and 2013
--demo 6   = putStrLn all films after "Zoe" says she is a fan of "The Reader"
--demo 66  = putStrLn all films after "Zoe" says she is a fan of "Skyfall"
--demo 7   = putStrLn average number of fans for films starring "Tom Hanks"
--demo 8   = putStrLn all co-stars of "Tom Hanks"

--
--
-- Your user interface code goes here
--
--
