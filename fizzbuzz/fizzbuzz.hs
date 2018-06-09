module FizzBuzz where

import System.Environment

fizzbuzz :: Integer -> String
fizzbuzz n
    | fizz && buzz = "FizzBuzz"
    | fizz = "Fizz"
    | buzz = "Buzz"
    | otherwise = show n
    where fizz = (n `mod` 3) == 0
          buzz = (n `mod` 5) == 0

fizzbuzz' :: [Integer -> String] -> Integer -> String
fizzbuzz' tests n
    | not (null output) = output
    | otherwise = show n
    where output = concat [ test n | test <- tests ]

fizzbuzzTest :: Integer -> String -> Integer -> String
fizzbuzzTest n s x = if (x `rem` n) == 0 then s else ""

fizzbuzzes :: [Integer -> String] -> [Integer] -> [String]
fizzbuzzes tests values = [ fizzbuzz' tests n | n <- values ]

fizz = fizzbuzzTest 3 "Fizz"
buzz = fizzbuzzTest 5 "Buzz"
jazz = fizzbuzzTest 7 "Jazz"

upperLimitFrom :: [String] -> Integer
upperLimitFrom [] = 100
upperLimitFrom args = read (args !! 0)

main = do
    args <- getArgs
    let num = upperLimitFrom args
        tests = [fizz, buzz, jazz] in
        putStrLn $ unwords $ fizzbuzzes tests [1..num]
