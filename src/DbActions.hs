{-# LANGUAGE OverloadedStrings #-}

module DbActions where

import qualified Data.Text as T
import qualified Data.Configurator as C
import qualified Control.Exception as E
import Database.PostgreSQL.Simple
import Data.String

import Queries
import DbObjects

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


hello :: IO Int
hello = do
  conn <- connectPostgreSQL "postgres://chpnweib:Mx7IPB1KjeBIjzFqV5Bv2jn5gbcgC4R6@hattie.db.elephantsql.com:5432/chpnweib"
  [Only i] <- query_ conn "select 2 + 2"
  return i

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
    query c qInsertUser $ (nm, sn, pc, pw, (T.pack pw)) :: IO [Only Int]


-- registerProgramOrDistribution = do


-- getProgramInfoByName = 

-- serching for Software entities strting from name entered
searchProgramms = do
  putStrLn "Enter the name or the name beginning of the program I'd like to find"
  nm <- getLine
  c <- dbconn
  query c qSearchProgramms $ Only (T.pack (nm ++ "%")) :: IO [Software]

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