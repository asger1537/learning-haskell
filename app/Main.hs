module Main where

import Lib
import Text.Printf (printf)

lowerUpper :: [(Char, Char)]
lowerUpper = zip ['a'..'z'] ['A'..'Z']

toUpper :: Char -> Char
toUpper c = if c == ' ' 
    then ' ' 
    else [b | (a, b) <- lowerUpper, a == c]!!0

toUpperString :: String -> String
toUpperString s = [toUpper c | c <- s]

toLower :: Char -> Char
toLower c = if c == ' ' 
    then ' ' 
    else [a | (a, b) <- lowerUpper, b == c]!!0

toLowerString :: String -> String
toLowerString s = [toLower c | c <- s]

getIndex :: Eq a => [a] -> a -> Int
getIndex a e = [i | (x, i) <- zip a [0..length a], x == e]!!0

{-
getIndexRec a e s 
    getIndexRec [] _ _ = error "empty list"
    if s >= length a then error "element not in list"
    if a!!s == e then s
    getIndexRec (tail a) e (s + 1)
-}

main :: IO ()
main = printf (toLowerString  "KAPPA")