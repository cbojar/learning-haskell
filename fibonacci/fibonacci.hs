module Fibonacci where

import System.Environment

fibonacci :: Integer -> Integer
fibonacci n
    | n < 0 = error "Cannot find a negative fibonacci number"
    | otherwise = fibHelper n 0 1
    where
        fibHelper :: Integer -> Integer -> Integer -> Integer
        fibHelper n a b
            | n == 0 = a
            | otherwise = fibHelper (n - 1) b (a + b)

firstNumberFrom :: [String] -> Integer
firstNumberFrom [] = 10
firstNumberFrom args = read $ args !! 0

main = do
    args <- getArgs
    let num = firstNumberFrom args in
        putStrLn $ show (fibonacci num)
