module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply _ = id

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = id

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- | Replaces all occurrences of wildcard in a list with the list given
--   as the third argument
--
-- Examples:
--
-- >>> substitute 'x' "x^2 + e^x + (2 + x + 2) - x" "1337"
-- "1337^2 + e^1337 + (2 + 1337 + 2) - 1337"
-- >>> substitute 1 [1,2,3,1,2] [0]
-- [0,2,3,0,2]
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard t s = foldr selector [] t
  where selector x acc | x == wildcard     = s ++ acc
                       | otherwise         = x : acc


-- | Tries to match two lists. The first parameter is the wildcard used in
--   matching. The second parameter is the pattern list that is used for matching.
--   The third parameter is the list in which the function looks for the pattern.
--   If a match is found then a sublist bound to the first occurrence of wildcard
--   in the pattern list is returned. If no match is found Nothing is returned.
--
--   Examples:
--
--   >>> match '*' "Hello*" "World"
--   Nothing
--   >>> match '*' "Lund *" "Lund University"
--   Just "University"
--   >>> match '*' "Lund *" "Gote University"
--   Nothing
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _  = Nothing
match _ _ []  = Nothing
match wildcard (p:ps) (s:ss)
    | p /= wildcard && p /= s           = Nothing
    | p /= wildcard && p == s           = match wildcard ps ss
    | otherwise                         = orElse swm lwm
  where swm = singleWildcardMatch wildcard (p:ps) (s:ss)
        lwm = longerWildcardMatch wildcard (p:ps) (s:ss)

-- | Helper function for matching. This considers the cast where wc must be equal
--   to wildcard. Returns the wildcard bound to the first element in the search list
--   if the rest of the two lists matches. Returns Nothing otherwise.
--
--   Examples:
--
--   >>> singleWildcardMatch '*' "*do" "bdo"
--   Just "b"
--   >>> singleWildcardMatch '*' "*do" "dobedo"
--   Nothing
singleWildcardMatch :: Eq a => a -> [a] -> [a] -> Maybe [a]
singleWildcardMatch wildcard (wc:ps) (x:xs) = mmap (\_ -> [x]) $ match wildcard ps xs

-- | Helper function for matching. Considers the case where the wildcard is
--   bound to x appended to possible more elements found by calling match.
--
--   Examples:
--
--   >>> longerWildcardMatch '*' "*do" "dobedo"
--   Just "dobe"
longerWildcardMatch :: Eq a => a -> [a] -> [a] -> Maybe [a]
longerWildcardMatch wildcard ps (x:xs) = mmap (x:) $ match wildcard ps xs



-- Test cases --------------------

--testPattern =  "a=*;"
--testSubstitutions = "32"
--testString = "a=32;"

--substituteTest = substitute '*' testPattern testSubstitutions
--substituteCheck = substituteTest == testString

--matchTest = match '*' testPattern testString
--matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
