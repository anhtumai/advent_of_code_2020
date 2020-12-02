{-# LANGUAGE TemplateHaskell #-}

import System.IO
import Control.Monad
import Data.List
import Data.List.Split

main = do
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let list = splitOn "\n" contents
        let count = foldl (\acc x -> if validate $ parse x then acc + 1 else acc) 0 list
        print count
        hClose handle

parse :: String -> (Int, Int, Char, String)
parse "" = (0,0, '\0', "")
parse string = (read x :: Int,read y :: Int,head z,t)
    where [x,y,z,t] = splitOneOf " -" string

validate :: (Int, Int, Char, String) -> Bool
validate (0,0, '\0', "") = False
validate (min, max, char, xs) = a /= b
        where a = (xs !! (min-1)) == char
              b = (xs !! (max-1)) == char
