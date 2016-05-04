{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe site

-- GET    /users
-- GET    /users/:id
-- POST   /users
-- PUT    /users/:id
-- DELETE /users/:id

-- GET    /users/:id/todos
-- GET    /users/:id/todos/:id
-- POST   /users/:id/todos
-- PUT    /users/:id/todos/:id
-- DELETE /users/:id/todos/:id

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ]

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
