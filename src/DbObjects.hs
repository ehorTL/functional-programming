{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedRecordFields #-}


module DbObjects where

import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow


data User = User {
    id:: Int,
    first_name:: String, 
    last_name:: String, 
    passport_code:: String,
    password_hash:: String} deriving (Show, Eq, Generic)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field

data Software = Software {
    id:: Int,
    name_ :: String,
    terms_and_conditions:: String,
    author:: String} deriving (Show, Eq, Generic)

instance FromRow Software where
    fromRow = Software <$> field <*> field <*> field <*> field

data SoftDistribution = SoftDistribution {
    id :: Int,
    software_id :: Int,
    version_ :: String,
    path_to_distribution :: String,
    license_from :: String,
    license_to :: String
} deriving (Show, Eq, Generic)

instance FromRow SoftDistribution where
    fromRow = SoftDistribution <$> field <*> field <*> field <*> field <*> field <*> field

data UserDistributionDownloads = UserDistributionDownloads{
    id :: Int,
    user_id :: Int,
    distribution_id :: Int,
    downloaded_on_date :: String 
} deriving (Show, Eq, Generic)

instance FromRow UserDistributionDownloads where
    fromRow = UserDistributionDownloads <$> field <*> field <*> field <*> field

data Statistics = Statistics {
    id :: Int,
    distribution_id :: Int,
    downloaded_by_users_times :: Int} deriving (Show, Eq, Generic)

instance FromRow Statistics where
    fromRow = Statistics <$> field <*> field <*> field