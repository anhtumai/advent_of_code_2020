{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = splitOn "\n\n" contents
  let requiredFields = [("byr", checkBYR), ("iyr", checkIYR), ("eyr", checkEYR), ("hgt", checkHGT), ("hcl", checkHCL), ("ecl", checkECL), ("pid", checkPID)]
  let answer = foldl (\count x -> if isValid x requiredFields then count + 1 else count) 0 list
  print answer

isValid :: String -> [(String, String -> Bool)] -> Bool
isValid s rfs = checkFieldsExistAndValid rfs (parsePassport s)

parsePassport :: String -> [(String, String)]
parsePassport string = map f (splitOneOf " \n" string)
  where
    f s = (head l, l !! 1) where l = splitOn ":" s

checkFieldsExistAndValid :: [(String, String -> Bool)] -> [(String, String)] -> Bool
checkFieldsExistAndValid rfs xs = foldr (\rf -> (&&) (fieldExistAndValid rf xs)) True rfs
  where
    fieldExistAndValid :: (String, String -> Bool) -> [(String, String)] -> Bool
    fieldExistAndValid rf [] = False
    fieldExistAndValid (requiredField, rule) ((field, string) : xs)
      | requiredField == field = rule string
      | otherwise = fieldExistAndValid (requiredField, rule) xs

checkBYR :: String -> Bool
checkBYR s = isStringNumber s && (x >= 1920) && (x <= 2002)
  where
    x = read s :: Int

checkIYR :: String -> Bool
checkIYR s = isStringNumber s && (x >= 2010) && (x <= 2020)
  where
    x = read s :: Int

checkEYR :: String -> Bool
checkEYR s = isStringNumber s && (x >= 2020) && (x <= 2030)
  where
    x = read s :: Int

checkHGT :: String -> Bool
checkHGT s
  | metric == "cm" && isStringNumber height && (h >= 150) && (h <= 193) = True
  | metric == "in" && isStringNumber height && (h >= 59) && (h <= 76) = True
  | otherwise = False
  where
    len = length s
    metric = drop (len - 2) s
    height = take (len - 2) s
    h = read height :: Int

checkHCL :: String -> Bool
checkHCL xs | length xs /= 7 || head xs /= '#' = False
checkHCL xs = checkHCL' (drop 1 xs)
  where
    checkHCL' "" = True
    checkHCL' (x : xs)
      | isDigit x = ('0' <= x) && (x <= '9') && checkHCL' xs
      | otherwise = ('a' <= x) && (x <= 'f') && checkHCL' xs

checkECL :: String -> Bool
checkECL x = checkECL' x ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  where
    checkECL' s [] = False
    checkECL' s (x : xs) = (s == x) || checkECL' s xs

checkPID :: String -> Bool
checkPID x = isStringNumber x && (length x == 9)

isStringNumber :: String -> Bool
isStringNumber "" = False
isStringNumber [x] = isDigit x
isStringNumber (x : xs) = isDigit x && isStringNumber xs
