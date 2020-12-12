{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = init $ splitOn "\n" contents
  let pairs = map parseRule lines
  print $ calculate "shinygold" pairs

-- calculate
calculate :: String -> [(String, [(Int, String)])] -> Int
calculate s pairs = total
  where
    value = fromMaybe [] $ lookup s pairs
    total = foldl f 0 value
      where
        f acc (count, bag) = acc + count * (1 + calculate bag pairs)

-- parse one line into an association list
-- "light red bags contain 1 bright white bags, 2 muted yellow bags." -> ("lightred",["brightwhite","mutedyellow"])
parseRule :: String -> (String, [(Int, String)])
parseRule a = (key, value)
  where
    keyValue = splitOn " contain " a
    key = concat $ init $ words $ head keyValue
    value = parseContains $ last keyValue

-- "2 shiny gold bags, 9 faded blue bags." -> ["shinygold", "fadedblue"]
parseContains :: String -> [(Int, String)]
parseContains "no other bags." = []
parseContains lines = map f bags
  where
    bags = splitOn ", " lines
    f bag = (read (head ws) :: Int, concat $ init $ drop 1 ws)
      where
        ws = words bag
