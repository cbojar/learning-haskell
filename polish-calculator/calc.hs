module PolishCalculator where

import Data.Char (isDigit)

operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (div))]

isNumber :: String -> Bool
isNumber term = foldl (&&) False (map isDigit term)

isOperator :: String -> Bool
isOperator = (`elem` [ os | (os,_) <- operators ])

calculate :: [Integer] -> [String] -> Integer
calculate [] [] = 0
calculate [number] [] = number
calculate numbers [] = error $ "More operators than operands: " ++ (show numbers)
calculate numbers (term:terms)
    | isNumber term = calculate ((read term):numbers) terms
    | isOperator term = calculate (calculateOperator term numbers) terms
    | otherwise = error $ "Unexpected term \"" ++ term ++"\""

calculateOperator :: String -> [Integer] -> [Integer]
calculateOperator operator [] = error $ "No operands for operator " ++ operator
calculateOperator operator [n] =
    error $ "Not enough operands for operator " ++ operator
calculateOperator operator (second:first:numbers) =
    (doOperation operator first second):numbers

doOperation :: String -> Integer -> Integer -> Integer
doOperation o a b = (head [ op | (os,op) <- operators, o == os ]) a b

main = do
    line <- getLine
    if null line then
        return ()
    else do
        print $ calculate [] (words line)
        main
