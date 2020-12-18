{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/11
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let seats = init $ splitOn "\n" contents
      answer = getNumberOfSeats seats

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
      | c == 'L' && checkEmptyChange (x, y) visibleSeats = '#'
      | c == '#' && checkOccuppiedChange (x, y) visibleSeats = 'L'
      | otherwise = c
      where
        c = (seats !! y) !! x
        visibleSeats = getVisibleSeats (x, y) seats

-- | Check if empty seat (L) can be changed to occupied (#)
-- | Condition: there are no occupied seats adjacent to it
checkEmptyChange :: (Int, Int) -> [Char] -> Bool
checkEmptyChange (x, y) visibleSeats = '#' `notElem` visibleSeats

-- | Check if occupied seat (#) can be changed to empty (L)
-- | Condition: four or more seats adjacent to it are also occupied
checkOccuppiedChange :: (Int, Int) -> [Char] -> Bool
checkOccuppiedChange (x, y) visibleSeats = length (filter (== '#') visibleSeats) >= 5

-- | Get status (empty or occupied) (exp: "L##") of all visible seats in 8 directions
getVisibleSeats :: (Int, Int) -> [[Char]] -> [Char]
getVisibleSeats (x, y) seats = foldl (\acc x -> if isJust x then fromJust x : acc else acc) [] $ map (getNearestSeat seats (x, y)) directions
  where
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

-- | Get status of the first seat a person can see in 1 direction (exp: Just 'L' or Just '#' or Nothing)
getNearestSeat :: [[Char]] -> (Int, Int) -> (Int, Int) -> Maybe Char
getNearestSeat seats (x, y) (directionX, directionY)
  | newX >= maxX || newY >= maxY || newX < 0 || newY < 0 = Nothing
  | ((seats !! newY) !! newX) /= '.' = Just ((seats !! newY) !! newX)
  | otherwise = getNearestSeat seats (newX, newY) (directionX, directionY)
  where
    newX = x + directionX
    newY = y + directionY
    maxX = length (head seats)
    maxY = length seats
