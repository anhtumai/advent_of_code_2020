{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import qualified Data.IntMap.Lazy as Map
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord (comparing)
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/16

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let info = init $ splitOn "\n" contents
      rules = parseRules info
      nearbyTickets = parseNearbyTickets info
      yourTicket = parseYourTicket info

  let validTickets = filter (checkValidTicket rules) nearbyTickets

  let possiblePositionss = getPossiblePositions rules (transpose validTickets)

  let exactPositions = determineExactPosition possiblePositionss

  let depaturePositions = take 6 exactPositions

  let answer = foldl (\acc x -> acc * (yourTicket !! x)) 1 depaturePositions

  print answer

-- give a list of possible positions, determine the exact position
-- Exp: determineExactPosition [[1,2,3],[2,3],[3]] -> [1,2,3]
determineExactPosition :: [[Int]] -> [Int]
determineExactPosition positionss = exactPositions
  where
    indexPositionsPair = zip [0 ..] positionss
    (indices, sortedPositionssByLength) = unzip $ sortOn (\(index, xs) -> length xs) indexPositionsPair
    sortedPositionssByIndex = foldl (\acc xs -> acc ++ (xs \\ acc)) [] sortedPositionssByLength
    exactPositions = map snd $ sortOn fst $ zip indices sortedPositionssByIndex

-- give the list of 0th,1st,..,Nth values of all tickets, get the possible rule position.
-- Exp: getPossiblePositions [[(0,1),(4,19)],[(0,5),(8,19)],[(0,13),(16-19)]] [[3,15,5],[9,1,4],[18,5,9]] -> [[1],[0],[2]]
-- explanation: all 3,15,5 satisfy rule with index 1 ([(0,5),(8,19)])
getPossiblePositions :: [[(Int, Int)]] -> [[Int]] -> [[Int]]
getPossiblePositions [] _ = []
getPossiblePositions (rules : ruless) tickets = fields : getPossiblePositions ruless tickets
  where
    fields = findIndices (all (checkValidField rules)) tickets

-- given list of rules and list of field in 1 ticket, check if that ticket is valid
checkValidTicket :: [[(Int, Int)]] -> ([Int] -> Bool)
checkValidTicket rules = all (checkValidField $ concat rules)

-- given list of rules and 1 field, check if that field satisfies at least one rule
checkValidField :: [(Int, Int)] -> Int -> Bool
checkValidField [] field = False
checkValidField ((min, max) : rules) field
  | field >= min && field <= max = True
  | otherwise = checkValidField rules field

-- parse all lines of a note to list of each nearby ticket
parseNearbyTickets :: [String] -> [[Int]]
parseNearbyTickets lines = map (map (\x -> read x :: Int) . splitOn ",") xss
  where
    xss = drop (fromJust (elemIndex "nearby tickets:" lines) + 1) lines

-- parse all lines of a note to list of rules
parseRules :: [String] -> [[(Int, Int)]]
parseRules lines = map parseRule rules
  where
    rules = take (fromJust (elemIndex "your ticket:" lines) - 1) lines

-- parse rule
-- "class: 1-3 or 5-7" -> ("class", [(1,3),(5,7)])
parseRule :: String -> [(Int, Int)]
parseRule string = values
  where
    line = splitOn ": " string
    field = head line
    rules = splitOn " or " $ last line
    values = map (\rule -> let x = splitOn "-" rule in (read (head x) :: Int, read (last x) :: Int)) rules

-- parse your ticket
parseYourTicket :: [String] -> [Int]
parseYourTicket lines = map (\x -> read x :: Int) $ splitOn "," $ lines !! (fromJust (elemIndex "your ticket:" lines) + 1)
