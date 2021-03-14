{-# LANGUAGE OverloadedStrings #-}

module DbActions where

import qualified Data.Text as T
import qualified Data.Configurator as C
import qualified Control.Exception as E
import Database.PostgreSQL.Simple
import Data.String
import Control.Exception

import Queries
import DbObjects
import Util

conf = do
  dbconf <- C.load [C.Required "db.conf"]
  return dbconf

url = conf >>= \x -> C.lookup x (T.pack "database.url") :: IO (Maybe String)

urlString :: IO String
urlString = url >>= \x -> case x of   
  Just y ->  return y :: IO String
  _ -> return "" :: IO String

dbconn :: IO Connection
dbconn = urlString >>= \x -> connectPostgreSQL $ fromString x

authorize :: String -> String -> IO Bool
authorize login password = return True :: IO Bool

-- user registration function
registerUser = do
    putStrLn "Enter the data proposed\nName:"
    nm <- getLine
    putStrLn "Surname:"
    sn <- getLine
    putStrLn "Passport code:"
    pc <- getLine
    putStrLn "Password:"
    pw <- getLine
    c <- dbconn  
    d:xs <- query c qInsertUser $ (nm, sn, pc, pw, (T.pack pw)) :: IO [Only Int]
    return $ (\x-> case x of 
      Only y -> y
      _ -> 0) d


registerProgramOrDistribution = do
  putStrLn "Add (n)ew software or (s)earch to add distribution?"
  nors <- getLine
  case nors of 
    "s" -> do
      softs <- searchProgramms
      putStrLn $ softListToString softs
      putStrLn "Enter the number of SOFTWARE unit you want to attach distribution to. Or 'q' to quit."
      inp <- getLine
      case inp of
        "q" -> undefined
        n -> do
          putStrLn "Enter software version, path to distribution, license FROM date, license TO date"
          vrsn <- getLine
          pathToDistr <- getLine
          lcnsFrom <- getLine
          lcnsTo <- getLine
          let nInt = read inp :: Int
          c <- dbconn
          sdid:_ <- query c qInsertSoftDistribution $ ((ids $ softs !! nInt), vrsn, pathToDistr, 
            lcnsFrom, lcnsTo) :: IO [Only Int]
          case sdid of
            Only newId -> return $ "Distribution added, ID: " ++ show newId
    "n" -> do
      putStrLn "Enter software name, terms and conditions, author, version, \
        \path to distribution, license FROM date, license TO date"
      nm <- getLine
      tc <- getLine
      athr <- getLine
      vrsn <- getLine
      pathToDistr <- getLine
      lcnsFrom <- getLine
      lcnsTo <- getLine
      c <- dbconn
      sid:_ <- query c qInsertSoftware $ (nm, tc, athr) :: IO [Only Int]
      case sid of
        Only newSoftId -> do
          sdid:_ <- query c qInsertSoftDistribution $ (newSoftId, vrsn, pathToDistr, 
            lcnsFrom, lcnsTo) :: IO [Only Int]
          case sdid of 
            Only newSDId -> putStrLn $ "Distribution registered, ID: " ++ show newSDId ++ 
              "Software info added, ID: " ++ show newSoftId
      return "str" :: IO String
    _ -> return "str" :: IO String


-- serching for Software entities strting from name entered
searchProgramms = do
  putStrLn "Enter the name or the name beginning of the program I'd like to find"
  nm <- getLine
  c <- dbconn
  query c qSearchProgramms $ Only (T.pack (nm ++ "%")) :: IO [Software]

searchProgrammsAndDistributions = do
  progs <- searchProgramms :: IO [Software]
  putStrLn $ softListToString progs
  putStrLn "Enter the number of software or 'q' to quit"
  inp <- getLine
  case inp of 
    "q" -> undefined
    n -> do
      let nInt = read n :: Int
      c <- dbconn
      query c qSelectDistributions $ Only (ids $ progs !! nInt) :: IO [SoftDistribution]

searchAndDownloadDistribution = do
  distrs <- searchProgrammsAndDistributions :: IO [SoftDistribution]
  putStrLn $ distrListToString distrs
  putStrLn "Enter the number of distribution to download:"
  n <- try $ read <$> getLine :: IO (Either SomeException Int)
  return "str"
  

getStatistics = do
  putStrLn "Show\n1. All\n2. For distribution\nq - quit"
  s <- getLine
  case s of 
    "1" -> do 
      c <- dbconn 
      query_ c qSelectStatisticsAll :: IO [Statistics]
    "2" -> do 
      progs <- searchProgramms :: IO [Software]
      n <- read <$> getLine :: IO Int
      undefined
      -- progs[n] 
    "q" -> undefined
    _ -> undefined

showStatistics = do
  putStrLn "Show\n1. All\n2. For distribution\nq - quit"