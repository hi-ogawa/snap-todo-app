{-# LANGUAGE OverloadedStrings #-}
module Db (pgConn) where

import Database.PostgreSQL.Simple

pgConn :: IO Connection
pgConn = connect . read =<< readFile "db/config.txt"

-- TODO:
--  - wrap `Connection` as environment of Snap monad
--  - should have explicit initializing step
--  - abstract with MonadIo
