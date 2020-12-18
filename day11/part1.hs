{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

-- Problem: Part 1 in https://adventofcode.com/2020/day/11
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let seats = init $ splitOn "\n" contents
      answer = getNumberOfSeats seats
  print seats
  print answer

-- | get number of seats when the chaos stabilizes
getNumberOfSeats :: [[Char]] -> Int
getNumberOfSeats seats
  | newSeats == seats = foldl (\acc x -> acc + length (filter (== '#') x)) 0 newSeats
  | otherwise = getNumberOfSeats newSeats
  where
    newSeats = nextRound seats

-- | get the status of seat in the next round
nextRound :: [[Char]] -> [[Char]]
nextRound seats = map (changeRow seats) [0 .. (length seats -1)]

changeRow :: [[Char]] -> Int -> [Char]
changeRow seats y = map f [0 .. (rowLength -1)]
  where
    rowLength = length (head seats)
    f x
      | c == 'L' && checkEmptyChange (x, y) seats = '#'
      | c == '#' && checkOccuppiedChange (x, y) seats = 'L'
      | otherwise = c
      where
        c = (seats !! y) !! x

-- | Check if empty seat (L) can be changed to occupied (#)
-- | Condition: there are no occupied seats adjacent to it
checkEmptyChange :: (Int, Int) -> [[Char]] -> Bool
checkEmptyChange (x, y) seats = foldl f True $ getAdjacentSeats (x, y) maxX maxY
  where
    maxX = length (head seats) - 1
    maxY = length seats - 1
    f acc (x, y) = (((seats !! y) !! x) /= '#') && acc

-- | Check if occupied seat (#) can be changed to empty (L)
-- | Condition: four or more seats adjacent to it are also occupied
checkOccuppiedChange :: (Int, Int) -> [[Char]] -> Bool
checkOccuppiedChange (x, y) seats = length (filter f $ getAdjacentSeats (x, y) maxX maxY) >= 4
  where
    maxX = length (head seats) - 1
    maxY = length seats - 1
    f (x, y) = ((seats !! y) !! x) == '#'

getAdjacentSeats :: (Int, Int) -> Int -> Int -> [(Int, Int)]
getAdjacentSeats (x, y) maxX maxY = filter (\(a, b) -> maxX >= a && a >= 0 && maxY >= b && b >= 0) l
  where
    l = [(x - 1, y - 1), (x -1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y -1), (x + 1, y), (x + 1, y + 1)]
