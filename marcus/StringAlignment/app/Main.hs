module Main where

import Lib
import Control.Monad
import System.IO
import Control.Applicative

main :: IO ()
main = forever $ do
    hSetBuffering stdout NoBuffering
    putStr "scoreMatch: "
    a <- read <$> getLine
    putStr "scoreMismatch: "
    b <- read <$> getLine
    putStr "scoreSpace: "
    c <- read <$> getLine
    putStr "First string: "
    s <- getLine
    putStr "Second string: "
    t <- getLine
    putStr ("Score is: " ++ show (similarityScore' a b c s t) ++ "\nAlignments:\n")
    putStr $ outputOptAlignments a b c s t
    putStr "\n***************************\n"
