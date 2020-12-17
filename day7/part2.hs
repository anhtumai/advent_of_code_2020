{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/7
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = init $ splitOn "\n" contents
  let pairs = map parseRule lines
      answer = calculate "shinygold" pairs
  print answer

-- | The fucntion calculates how many bags the input bag can contain
-- "inputBag" [("inputBag",[(1,"X1"),(2,"X2)"]), ("X1",[(3,"X3"),(4,"X4)"])] -> 1 + 1*(3+4) + 2 + 2*0 = 10
calculate :: String -> [(String, [(Int, String)])] -> Int
calculate s pairs = foldl f 0 value
  where
    value = fromMaybe [] $ lookup s pairs
    f acc (count, bag) = acc + count * (1 + calculate bag pairs)

-- parse data

-- parse one line of information into a tuple (key-value pair)
-- "light red bags contain 1 bright white bags, 2 muted yellow bags." -> ("lightred",[(1,"brightwhite"),(2,"mutedyellow)"])
parseRule :: String -> (String, [(Int, String)])
parseRule a = (key, value)
  where
    keyValue = splitOn " contain " a
    key = concat $ init $ words $ head keyValue
    value = parseContains $ last keyValue

-- parse 1 string into list of bag names
-- "2 shiny gold bags, 9 faded blue bags." -> [(2,"shinygold"), (9,"fadedblue")]
-- "no other bags." -> []
parseContains :: String -> [(Int, String)]
parseContains "no other bags." = []
parseContains lines = map f bags
  where
    bags = splitOn ", " lines
    f bag = (read (head ws) :: Int, concat $ init $ drop 1 ws)
      where
        ws = words bag
