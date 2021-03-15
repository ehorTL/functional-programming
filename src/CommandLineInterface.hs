module CommandLineInterface (cli) where
-- module CommandLineInterface where

import Data.List
import System.Exit as Se
import Text.Read

import DbActions

someop :: IO ()
someop = return ()

actions = ["1. Register user", 
    "2. Register program or distribution", 
    "3. Search programms / download distribution",
    "4. Show statistics",
    "q - quit"]

cli :: IO ()
cli = do 
    putStrLn $ "Enter the action you wold like to perform:\n" ++ (intercalate "\n" actions)
    option1 <- getLine
    putStrLn "\nHandling input...\n"
    case option1 of
        "1" -> do 
            uid <- registerUser
            putStrLn $ "User registered, ID: " ++ (show uid)
        "2" -> registerProgramOrDistributionV2
        "3" -> searchAndDownloadDistribution
        "4" -> do 
            showStatistics
        "q" -> do
            putStrLn "Exit. The program is terminating..."
            Se.exitSuccess
        _ -> do
            putStrLn "Command not recognized. Exit."
            Se.exitSuccess
    putStrLn "\nChoose:\nq - quit\nr - start REPL again"
    option2 <- getLine
    case option2 of
        "r" -> do
            putStrLn "\n\n"
            cli
        _ -> putStrLn "Exit."
