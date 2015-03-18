-- 
-- MATHFUN - Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2014/15
-- UP683693
--

import Data.List
import Text.Printf

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

testDatabase :: [Film]
testDatabase = [ Film "Casino Royale" ["Daniel Craig", "Eva Green", "Judi Dench"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"], Film "Cowboys & Aliens" ["Harrison Ford", "Daniel Craig", "Olivia Wilde"] 2011 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"], Film "Catch Me If You Can" ["Leonardo DiCaprio", "Tom Hanks"] 2002 ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"], Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan", "Colin Firth"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"], Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"], Film "Quantum of Solace" ["Daniel Craig", "Judi Dench"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"], Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"], Film "Collateral" ["Tom Cruise", "Jamie Foxx"] 2004 ["Dave", "Garry", "Megan", "Sam", "Wally"], Film "The Departed" ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"] 2006 ["Zoe", "Emma", "Paula", "Olga", "Dave"], Film "Up in the Air" ["George Clooney", "Vera Farmiga"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"], Film "Gravity" ["George Clooney", "Sandra Bullock"] 2013 ["Zoe", "Emma", "Garry", "Ian", "Neal", "Wally", "Olga", "Dave"], Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"], Film "Ocean's Twelve" ["George Clooney", "Matt Damon", "Catherine Zeta-Jones", "Julia Roberts"] 2004 ["Jo", "Wally", "Emma"], Film "The Adjustment Bureau" ["Matt Damon", "Emily Blunt"] 2011 ["Kevin", "Tim", "Emma", "Emma", "Garry", "Ian", "Neal"], Film "Cloud Atlas" ["Tom Hanks", "Halle Berry"] 2012 ["Dave", "Amy", "Garry", "Ian", "Neal"], Film "The Reader" ["Kate Winslet", "Ralph Fiennes"] 2008 ["Emma", "Bill", "Dave", "Liz"], Film "Begin Again" ["Keira Knightley", "Mark Ruffalo", "James Corden"] 2013 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma"], Film "Revolutionary Road" ["Leonardo DiCaprio", "Kate Winslet"] 2008 ["Wally", "Sam", "Dave", "Jo"], Film "Into the Woods" ["Meryl Streep", "Emily Blunt", "James Corden"] 2014 ["Dave", "Jo", "Wally", "Emma"], Film "Now You See Me" ["Jesse Eisenberg", "Mark Ruffalo"] 2013 ["Bill", "Sam", "Zoe", "Jo"], Film "Larry Crowne" ["Tom Hanks", "Julia Roberts"] 2011 ["Liz", "Wally"], Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"], Film "Edge of Tomorrow" ["Tom Cruise", "Emily Blunt"] 2014 ["Jo", "Chris", "Wally", "Ian", "Garry", "Bill", "Olga", "Megan", "Sam"], Film "Django Unchained" ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"] 2012 ["Kevin", "Tim", "Emma", "Olga"], Film "Skyfall" ["Daniel Craig", "Judi Dench", "Ralph Fiennes"] 2012 ["Bill", "Olga", "Zoe", "Paula", "Megan", "Sam", "Wally" ]]

-- 
--
--  Your functional code goes here
--
--
addFilm :: [Film] -> Film -> [Film]
addFilm existingFilms newFilm = existingFilms ++ [newFilm]
    -- appends. : Would prepend

wordsToString :: [String] -> String
wordsToString [] = ""
wordsToString (x:xs)
    | length (x:xs) == 1    = x ++ "."
    | otherwise             = x ++ ", " ++ wordsToString xs

filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((Film name cast year fans) : films) = name ++ " (" ++ show year ++ ")\n" ++ showCast cast ++   showFans fans ++ filmsAsString films
    where
        showCast cast = "Starring: " ++ wordsToString cast ++ "\n"

        showFans fans = show (length fans) ++ " fans" ++ "\n\n"
    -- putStrLn $ filmsAsString testDatabase

userIsFanOf :: [Film] -> String -> [Film]
userIsFanOf films fan = filter (\(Film name cast year fans) -> elem fan fans) films

findFilm :: [Film] -> String -> Film
findFilm films film = case (find (\(Film name cast year fans) -> name == film ) films ) of
        Nothing -> (Film "" [] 0 [])
        Just x  -> x

allFansOf :: [Film] -> Name -> Fans
allFansOf films film = getFans $ findFilm films film
    where
        -- Extract fans from Film
        getFans :: Film -> Fans
        getFans (Film name cast year fans) = fans

filmsInPeriod :: [Film] -> Int -> Int -> [Film]
--filmsInPeriod films min max =  [(Film name cast year fans) | (Film name cast year fans) <- films, min <= year && year <= max]
filmsInPeriod films min max =  filter (\(Film name cast year fans) -> (min <= year && year <= max)) films
    -- putStrLn $ filmsAsString  $ filmsInPeriod testDatabase 2010 2015

-- VI allow a user to say they are a fan of a particular film
--becomeFilmFan :: [Film] -> String -> [Film]
--becomeFilmFan films film fan = (extract fan list in new Film) . findFilm films film : (film without target)
--Extract fans, append, put back

-- VII. give the average number of fans for the films starring a particular actor
fanAverage:: [Film] -> String -> Float
fanAverage films actor = average [length $ fans | (Film name cast year fans) <- films, elem actor cast] 
    where average fanList = fromIntegral ( sum fanList) / fromIntegral ( length fanList)
--putStrLn $ show $ fanAverage testDatabase "Daniel Craig"


-- VIII. give (without duplicates) the names of actors who have co-starred in at least one film with a particular actor
coStarsOf :: [Film] -> String -> [String]
coStarsOf [] actor = []
coStarsOf ((Film name cast year fans) : films) actor = nub $ filmCoStars cast actor ++ coStarsOf films actor

-- putStrLn $ show $coStarsOf testDatabase "Daniel Craig"


filmCoStars :: Cast -> String -> Cast
-- co-Stars of given actor or [] if not staring actor
filmCoStars cast actor
    | elem actor cast   = filter (/=actor) cast
    | otherwise         = []

-- distinct cast : elem actor cast concat ! list eval because nested

-- elem actor cast reused. Factor out?

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
    -- addFilm testDatabase aNewFilm
demo 1   = putStrLn $ filmsAsString $ addFilm testDatabase (Film "The Monuments Men" ["George Clooney", "Matt Damon", "Bill Murray"] 2014 [] )
--          all films after adding 2014 film "The Monuments Men" 
--                   starring "George Clooney", "Matt Damon" and "Bill Murray" 
--                   to testDatabase
demo 2   = putStrLn $ filmsAsString testDatabase
demo 3   = putStrLn $ filmsAsString  $ userIsFanOf testDatabase "Zoe"
            --putStrLn all films that Zoe is a fan of
demo 4   = putStrLn $ wordsToString $ allFansOf testDatabase "Titanic"
--              putStrLn all fans of Titanic
demo 5   = putStrLn $ filmsAsString  $ filmsInPeriod testDatabase 2010 2013
--              putStrLn all films between 2010 and 2013
--demo 6   = putStrLn all films after "Zoe" says she is a fan of "The Reader"
--demo 66  = putStrLn all films after "Zoe" says she is a fan of "Skyfall"
demo 7   = putStrLn $ show $ fanAverage testDatabase "Tom Hanks"
--              putStrLn average number of fans for films starring "Tom Hanks"
demo 8   = putStrLn $ show $coStarsOf testDatabase "Tom Hanks"
    --putStrLn all co-stars of "Tom Hanks"
--
--
-- Your user interface code goes here
--
--

getString :: String -> IO String
getString prompt = do
    putStrLn prompt
    getLine


getInt :: String -> IO Int
getInt prompt = do 
    putStrLn prompt
    str <- getLine
    return (read str :: Int)


startUp :: IO ()
startUp = do
    content <- readFile "films.txt"
    let filmList = read content :: [Film]
    finalFilmList <- menu filmList
    writeFile "films.txt" $ show finalFilmList
    putStrLn "File saved"

menu :: [Film] -> IO [Film]
menu filmList = do
    putStrLn "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n1. Add a new film to the database.\n2. Give all films in the database.\n3. Give all films that a particular user is a fan of.\n4. Give all fans of a particular film.\n5. Give all the films that were released during a particular period (i.e. between a given start year and end year).\n6. Allow a user to say they are a fan of a particular film.\n7. Give the average number of fans for the films starring a particular actor.\n8. Give (without duplicates) the names of actors who have co-starred in at least one film with a particular actor.\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    choice <- getLine
    case choice of
        "1" -> do
            name <- getString "Please enter film name: "
            cast <- getString "Please enter film cast: "   -- To change
            year <- getInt "Please enter film year: "
            let newList = addFilm filmList (Film name (read cast :: Cast) year [])
            menu newList

        "2" -> do
            putStrLn $ filmsAsString filmList
            menu filmList

        "3" -> do
            fan <- getString "Choose a fan: "
            putStrLn $ filmsAsString $ userIsFanOf filmList fan
            menu filmList

        "4" -> do
            film <- getString "Choose a film: "
            putStrLn $ wordsToString $ allFansOf filmList film
            menu filmList

        "5" -> do
            min <- getInt "Choose minimum year: "
            max <- getInt "Choose maximum year: "
            putStrLn $ filmsAsString $ filmsInPeriod filmList min max
            menu filmList

        "7" -> do
            actor <- getString "Choose an actor: "
            --putStrLn $ show $ fanAverage filmList actor
            printf "%3.2f Fan average" $ fanAverage filmList actor
            menu filmList

        "8" -> do
            actor <- getString "Choose an actor: "
            putStrLn $ wordsToString $ coStarsOf filmList actor
            menu filmList

        _   -> return filmList
