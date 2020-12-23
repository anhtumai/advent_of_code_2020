{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import qualified Data.IntMap.Lazy as Map
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 1 in https://adventofcode.com/2020/day/16

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let info = init $ splitOn "\n" contents
      rules = take (fromJust (elemIndex "your ticket:" info) - 1) info
      nearbyTickets = map (\x -> read x :: Int) $ foldl (\acc x -> acc ++ splitOn "," x) [] $ drop (fromJust (elemIndex "nearby tickets:" info) + 1) info

      ruleFuncs = foldl (\acc rule -> acc ++ parseRule rule) [] rules

      checkInvalid ticket = foldl (\acc ruleFunc -> not (ruleFunc ticket) && acc) True ruleFuncs

      invalidTickets = filter checkInvalid nearbyTickets

  print $ sum invalidTickets
  hClose handle

parseRule :: String -> [Int -> Bool]
parseRule s = map f c
  where
    c = splitOn " or " $ last $ splitOn ": " s
    f :: String -> (Int -> Bool)
    f a = let b = map (\x -> read x :: Int) $ splitOn "-" a in (\x -> x >= head b && x <= last b)
