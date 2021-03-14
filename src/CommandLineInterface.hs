-- module CommandLineInterface (cli) where
module CommandLineInterface where

import Data.List
import DbActions

someop :: IO ()
someop = return ()

actions = ["1. Register user", 
    "2. Register program or distribution", 
    "3. Search programms", 
    "4. Show statistics",
    "q - quit"]

cli :: IO ()
cli = do 
    putStrLn $ "Enter the action you wold like to perform:\n" ++ (intercalate "\n" actions)
    option1 <- getLine
    case option1 of
        "1" -> do 
            uid <- registerUser
            putStrLn $ "User registered, ID: " ++ (show uid)
        "2" -> someop
        "3" -> someop
        "4" -> someop
        "5" -> do 
            showStatistics
        "q" -> undefined
        _ -> error "Unexpected input. Exit."
    putStrLn "Choose:\nq - quit\nr - start REPL again"
    option2 <- getLine
    case option2 of
        "r" -> cli
        _ -> undefined