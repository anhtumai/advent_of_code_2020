{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Debug.Trace
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = splitOn "\n" contents
  let answer = countTree list 0
  print answer
  hClose handle

countTree :: [String] -> Int -> Int
countTree [""] _ = 0
countTree [] _ = 0
countTree (x : xs) pos
  | x !! pos == '#' = 1 + countTree xs newPos
  | x !! pos == '.' = countTree xs newPos
  | null x = 0
  where
    len = length x
    newPos = if pos + 4 <= len then pos + 3 else pos + 3 - len
