{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo where

import qualified Data.Text as T
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Db (pgConn)

---------------------
-- data definition --

data Todo = Todo { key_id :: (Maybe Int), note :: T.Text, completed :: Bool}
            deriving (Show, Eq)

instance FromRow Todo where
  fromRow = Todo <$> (Just <$> field) <*> field <*> field

instance ToRow Todo where
  toRow (Todo Nothing n c) = [toField n, toField c]
  toRow (Todo (Just i) n c) = [toField n, toField c, toField i]


---------------
-- utilities --

persisted :: Todo -> Bool
persisted (Todo Nothing _ _) = False
persisted _                  = True

save :: Connection -> Todo -> IO Todo
save conn todo =
  head <$> query conn (if persisted todo
                       then qUpdate
                       else qInsert
                       ) todo
  where
    qInsert = "insert into todo (note, completed) values (?, ?) returning id, note, completed"
    qUpdate = "update todo set note = ?, completed = ? where id = ? returning id, note, completed"

delete :: Connection -> Int -> IO Todo
delete conn kId =
  head <$> query conn qDelete [kId]
  where
    qDelete = "delete from todo where id = ? returning id, note, completed"

find :: Connection -> Int -> IO Todo
find conn kId =
  head <$> query conn qSelect [kId]
  where
    qSelect = "select id, note, completed from todo where id = ?"

index :: Connection -> IO [Todo]
index conn =
  query_ conn qSelect
  where
    qSelect = "select id, note, completed from todo"


----------
-- spec --

ex0 :: IO ()
ex0 = do
  conn <- pgConn
  -- create
  t0 <- save conn (Todo Nothing "play with postgresql-simple" False)
  print t0
  -- update
  t1 <- save conn (t0 { completed = True })
  print t1
  -- read
  t2 <- find conn (fromJust (key_id t1))
  print t2
  -- delete
  delete conn (fromJust (key_id t2))
  -- read all
  ts <- index conn
  print ts
  close conn
