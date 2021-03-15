{-# LANGUAGE OverloadedStrings #-}

module DbActions where

import qualified Data.Text as T
import Text.Read as Tr
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

-- returns id of authorized user 
-- IO Either String Int
authorize = do
  putStrLn "Enter your passport code:"
  pc <- getLine
  putStrLn "Enter the password:"
  pw <- getLine
  c <- dbconn
  users <- query c qSelectUserByPass $ (pc, pw, (T.pack pw)) :: IO [User]
  case users of
    [] -> do 
      return (Left "Authorazion failed") 
    _ -> do 
      return (Right (idu $ head users))


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

-- searchAndDownloadDistribution :: IO String
searchAndDownloadDistribution = do
  distrs <- searchProgrammsAndDistributions :: IO [SoftDistribution]
  putStrLn $ distrListToString distrs
  putStrLn "Enter the number of distribution to download:"
  n <- Tr.readEither <$> getLine :: IO (Either String Int)
  case n of 
    Left x -> do
      putStrLn $ "Read error: " ++ x ++ "\nExit."
    Right num -> do
      putStrLn "Download the distribution? [y/n]"
      yn <- getLine
      case yn of
        "y" -> do
          let idSDist = idsd $ distrs !! num
          putStrLn "Authorization needed.\n"
          eitherUserId <- authorize
          case eitherUserId of
            Left message -> do
              putStrLn message
              putStrLn "Exit"
            Right userId -> do
              putStrLn "Successfully authorized"
              c <- dbconn
              distrStats <- query c qSelectStatById $ Only idSDist :: IO[Statistics]
              if distrStats == [] then do
                Only newStatId:_ <- query c qInsertStat1 $ Only idSDist :: IO [Only Int]
                putStrLn $ "Statistics updated. New record inserted, ID: " ++ show newStatId --log
                else do
                  rowsUpdated <- execute c qUpdateStat $ (Only (idSDist :: Int))
                  putStrLn $ "Stattistics updated. Records updated: " ++ show rowsUpdated --log
              Only userDownloadId:_ <- query c qInsertUserDistributionDownloads $ 
                (userId, idSDist) :: IO [Only Int]
              putStrLn $ "Distribution downloaded. Record ID: " ++ show userDownloadId
        _ -> do
              putStrLn "Exit"  

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