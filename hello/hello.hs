main = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn ("Hello there, " ++ name ++ "!")
