{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = init $ splitOn "\n" contents
  let pairs = map parseRule lines
      answer = Set.size $ search "shinygold" pairs
  print answer

-- print $ search "shinygold" a

search :: String -> [(String, [String])] -> Set.Set String
search s pairs = Set.fromList $ search' s pairs pairs

search' :: String -> [(String, [String])] -> [(String, [String])] -> [String]
search' s [] _ = []
search' s ((key, value) : pairs) originalPairs
  | s `elem` value = [key] ++ search' key originalPairs originalPairs ++ search' s pairs originalPairs
  | otherwise = search' s pairs originalPairs

-- parse one line into an association list
-- "light red bags contain 1 bright white bags, 2 muted yellow bags." -> ("lightred",["brightwhite","mutedyellow"])
parseRule :: String -> (String, [String])
parseRule a = (key, value)
  where
    keyValue = splitOn " contain " a
    key = concat $ init $ words $ head keyValue
    value = parseContains $ last keyValue

-- "2 shiny gold bags, 9 faded blue bags." -> ["shinygold", "fadedblue"]
parseContains :: String -> [String]
parseContains "no other bags." = []
parseContains lines = map f bags
  where
    bags = splitOn ", " lines
    f bag = concat $ init $ drop 1 (words bag)
