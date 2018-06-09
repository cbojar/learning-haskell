module PigLatin where

import Data.Char

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

areAnyLettersIn :: String -> Bool
areAnyLettersIn word = foldl (||) False (map isLetter word)

pigLatin :: String -> String
pigLatin [] = ""
pigLatin word@(c:cs)
    | not (areAnyLettersIn word) = word
    | isVowel c = word ++ "way"
    | otherwise = let
        (onlyWord, punctuation) = separateTrailingPunctuation (word, "")
        (prefix, suffix) = separateLeadingConsonants ("", onlyWord) in
        suffix ++ prefix ++ "ay" ++ punctuation

separateTrailingPunctuation :: (String, String) -> (String, String)
separateTrailingPunctuation ("", trailing) = ("", trailing)
separateTrailingPunctuation (leading, trailing)
    | isLetter c = (leading, trailing)
    | otherwise = separateTrailingPunctuation (init leading, c:trailing)
    where c = last leading

separateLeadingConsonants :: (String, String) -> (String, String)
separateLeadingConsonants (leading, "") = (leading, "")
separateLeadingConsonants (leading, trailing@(c:cs))
    | isVowel c = (leading, trailing)
    | otherwise = separateLeadingConsonants (leading ++ [c], cs)

main = do
    putStrLn "Enter a line to translate or an empty line to quit:"
    line <- getLine
    if null line then
        return ()
    else do
        print $ unwords $ map pigLatin $ words line
        main

