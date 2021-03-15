{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedRecordFields #-}


module DbObjects where

import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Prelude hiding (id)

data User = User {
    idu:: Int,
    first_name:: String, 
    last_name:: String, 
    passport_code:: String,
    password_hash:: String} deriving (Eq, Generic)

instance Show User where 
    show u = "User ID: " ++ (show $ idu u) ++ "\n" ++ 
        "First Name: " ++ first_name u ++ "\n" ++
        "Last Name: " ++ last_name u ++ "\n" ++
        "Passport Code: " ++ passport_code u ++ "\n"

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field

data Software = Software {
    ids:: Int,
    name_ :: String,
    terms_and_conditions:: String,
    author:: String} deriving (Eq, Generic)

instance Show Software where
    show s = "Software ID: " ++ (show $ ids s) ++ "\n" ++
        "Name: " ++ name_ s ++ "\n" ++
        "Terms And Conditions: " ++ terms_and_conditions s ++ "\n" ++
        "Author: " ++ author s ++ "\n"

instance FromRow Software where
    fromRow = Software <$> field <*> field <*> field <*> field

data SoftDistribution = SoftDistribution {
    idsd :: Int,
    software_id :: Int,
    version_ :: String,
    path_to_distribution :: String,
    license_from :: String,
    license_to :: String
} deriving (Eq, Generic)

instance Show SoftDistribution where 
    show d = "Software Distribution ID: " ++ (show $ idsd d) ++ "\n" ++
        "Software ID: " ++ (show $ software_id d) ++ "\n" ++
        "Version: " ++ version_ d ++ "\n" ++
        "Path To Distribution: " ++ path_to_distribution d ++ "\n" ++
        "License From: " ++ license_from d ++ "\n" ++
        "License To: " ++  license_to d ++ "\n" 

instance FromRow SoftDistribution where
    fromRow = SoftDistribution <$> field <*> field <*> field <*> field <*> field <*> field

data UserDistributionDownloads = UserDistributionDownloads{
    idudd :: Int,
    user_id :: Int,
    distribution_id :: Int,
    downloaded_on_date :: String 
} deriving (Show, Eq, Generic)

instance FromRow UserDistributionDownloads where
    fromRow = UserDistributionDownloads <$> field <*> field <*> field <*> field

data Statistics = Statistics {
    idstat :: Int,
    distribution_id_ :: Int,
    downloaded_by_users_times :: Int} deriving (Eq, Generic)

instance Show Statistics where
    show s = "ID: " ++ (show $ idstat s) ++ "\n" ++
        "Distribution ID: " ++ (show $ distribution_id_ s) ++ "\n" ++
        "Downloaded times: " ++ (show $ downloaded_by_users_times s) ++ "\n"

instance FromRow Statistics where
    fromRow = Statistics <$> field <*> field <*> field