-- module CommandLineInterface (cli) where
module CommandLineInterface where

import Data.List
import DbActions

someop :: IO ()
someop = return ()

actions = ["1. Register user", 
    "2. Register program or distribution", 
    "3. Get programm info by name",
    "4. Search programms", 
    "5. Show statistics", 
    "q - quit"]



cli :: IO ()
cli = do 
    putStrLn $ "Enter the action you wold like to perform:\n" ++ (intercalate "\n" actions)
    option1 <- getLine
    case option1 of
        "1" -> someop
        "2" -> someop
        "3" -> someop
        "4" -> someop
        "5" -> someop        
        "q" -> undefined
        _ -> error "Unexpected input. Exit."
