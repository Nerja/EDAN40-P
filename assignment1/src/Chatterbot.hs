module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

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
-- | Takes a BotBrain consisting of possible answer
--   patterns for different question patterns. This function
--   returns a function that takes a Phrase and formulates
--   an answer using one of the corresponding answer patterns.
--   The used answer pattern is picked at random from the associated
--   answer patterns.
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
    r <- randomIO :: IO Float
    return (rulesApply $ (map.map2) (id, pick r) brain)


-- | Function decomposition of two functions f and g.
--   function g is applied first and should take two parameters.
--   Function f is applied to the result of function g.
--   Function comes from "http://buffered.io/posts/point-free-style-what-is-it-good-for/"
(.^) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
(.^) = (.) . (.)

-- | Applies a list of pattern transformations on an input Phrase.
--   The intermediate result is reflected.
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = fromMaybe [] .^ transformationsApply "*" reflect

-- | Replaces each word in a phrase with its corresponding word in the map
--   given by reflections.
--
--   Examples:
--
--   >>> reflect ["am", "my"]
--   ["are","your"]
reflect :: Phrase -> Phrase
reflect = map $ try (`lookup` reflections)

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

-- | Compiles the given format to use the BotBrain format.
--   Basically changes all Strings to the Phrase format where
--   each word is a String and a ordinary sentences is encoded as several
--   strings. Before converting the format all characters is converted into
--   lowercase.
--
--   Examples:
--
--   >>> rulesCompile [("Hej *", ["Hello *", "Hi *"])]
--   [(["hej","*"],[["hello","*"],["hi","*"]])]
rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = (map.map2) (lowerWords, map lowerWords)
  where lowerWords = words . map toLower


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

-- | Applies a list of reductions to reduce similar statements
--   to the same statement. The function uses fixedpoint computation.
--   When no further reductions can be fund the result is returned.
--
--   Examples:
--
--   >>> reductionsApply [(["i","am","very","*"],["i","am","*"])] $ words "i am very very very tired"
--   ["i","am","tired"]
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . applyReductions
  where applyReductions = transformationsApply "*" id


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
substitute wildcard t s = concatMap picker t
  where picker x | x == wildcard    = s
                 | otherwise        = [x]


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
singleWildcardMatch wildcard (_:ps) (x:xs) = mmap (const [x]) $ match wildcard ps xs

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

-- | Tries to find a match between the from template and the
--   input m.a.p wildcard. If a match is found the result is
--   inserted into the to template and the result is returned.
--   If no match is found nothing is returned.
--
--   Examples:
--
--   >>> transformationApply '*' id "My name is Zacharias" ("My name is *", "Je m'appelle *")
--   Just "Je m'appelle Zacharias"
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard func input transPair = mmap (substitute wildcard substitute_template . func) matchResult
  where matchResult           = match wildcard search_template input
        substitute_template   = snd transPair
        search_template       = fst transPair


-- | Tries to apply a list of pattern transformations on the input.
--   If a pattern transformation is successful the result is returned without
--   trying to apply the rest. If no pattern transformation is successful then Nothing is returned.
--
--   Examples:
--
--   >>> transformationsApply '*' id [("Lol *", "Lulz *"), ("My name is *", "Ich heisse *")] "My name is Eliza"
--   Just "Ich heisse Eliza"
--   >>> transformationsApply '*' id [("Lol *", "Lulz *"), ("My house is *", "Ich heisse *")] "My name is Eliza"
--   Nothing
--   >>> transformationsApply '*' id [("* Lol", "* Lulz"), ("My name is *", "Ich heisse *")] "My name is Eliza, Lol"
--   Just "My name is Eliza, Lulz"
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wildcard func transformations input = foldl doApply Nothing transformations
  where doApply acc x = orElse acc $ transformationApply wildcard func input x
