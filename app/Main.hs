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


getIndexRec :: (Eq a, Integral b) => [a] -> a -> b -> b
getIndexRec a e i = if (head a) == e then i else getIndexRec (tail a) e (i+1)

getIndexRecWithGuards :: (Eq a, Integral b) => [a] -> a -> b -> b
getIndexRecWithGuards (x:xs) e i
    | x == e = i
    | otherwise = getIndexRecWithGuards xs e (i+1)


volumeHalfCircleToppedRect :: (RealFloat a) => a -> a -> a
volumeHalfCircleToppedRect w h d = areaHalfCircleToppedRect*d
    where areaHalfCircleToppedRect = 
        let rectArea = w*(h-w/2)
            halfCircleArea = pi*(w/2)^2/2
        in rectArea + halfCircleArea    
    


main :: IO ()
l :: [Char]
l = "Hej"
main = printf "Area: %f" ((areaHalfCircleToppedRect 5 15) :: Float)