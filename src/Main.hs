{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.Map.Lazy as Mp
import Snap.Core
import Snap.Http.Server (quickHttpServe)
import qualified Snap.Test as SnpT
import Test.Hspec

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


----------
-- spec --

spec_root :: Spec
spec_root = do
  describe "GET /" $ do
    it "." $ do
      let req = SnpT.get "/" Mp.empty
      resp <- SnpT.runHandler req site
      SnpT.getResponseBody resp >>= (`shouldBe` "hello world")
  describe "GET /foo" $ do
    it "." $ do
      let req = SnpT.get "/foo" Mp.empty
      resp <- SnpT.runHandler req site
      SnpT.getResponseBody resp >>= (`shouldBe` "bar")
  describe "GET /echo/:echoparam" $ do
    it "." $ do
      let req = SnpT.get "/echo/hey" Mp.empty
      resp <- SnpT.runHandler req site
      SnpT.getResponseBody resp >>= (`shouldBe` "hey")
