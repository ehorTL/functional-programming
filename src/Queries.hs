{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Database.PostgreSQL.Simple

qSelectUserByPass = "select * from users where passport_code=? and password_hash=crypt(?, ?)"

qInsertUser = "insert into users (first_name, last_name, passport_code, \
    \ password_hash) values (?, ?, ?, crypt(?, ?)) returning id;" :: Query

qInsertSoftware = "insert into software (name_, terms_and_conditions, author) values \
    \ (?, ?, ?) returning id;" :: Query

qInsertSoftDistribution = "insert into soft_distribution (software_id, version_, path_to_distribution, \
    \ license_from, license_to) values (?, ?, ?, ?, ?) returning id;" :: Query

qInsertUserDistributionDownloads = "insert into user_distribution_downloads (user_id, \
    \ distribution_id) values (?, ?) returning id;" :: Query

qUpdateStat = "update stat set distribution_id=?, downloaded_by_users_times=? where id=?;" :: Query
qInsertStat = "insert into stat (distribution_id, downloaded_by_users_times) values (?, 0) returning id;" :: Query

qSearchProgramms = "select * from software where name_ like ?" :: Query

qSelectStatisticsAll = "select * from stat" :: Query

