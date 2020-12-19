{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = splitOn "\n\n" contents
  let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  let answer = foldl (\count x -> if isValid x requiredFields then count + 1 else count) 0 list
  print answer
  hClose handle

isValid :: String -> [String] -> Bool
isValid s rfs = checkFieldsExist rfs (parsePassport s)

parsePassport :: String -> [(String, String)]
parsePassport string = map f (splitOneOf " \n" string)
  where
    f s = (head l, l !! 1) where l = splitOn ":" s

checkFieldsExist :: [String] -> [(String, String)] -> Bool
checkFieldsExist rfs xs =
  foldr (\rf -> (&&) (fieldExist rf xs)) True rfs
  where
    fieldExist rf [] = False
    fieldExist rf ((field, _) : xs)
      | rf == field = True
      | otherwise = fieldExist rf xs
