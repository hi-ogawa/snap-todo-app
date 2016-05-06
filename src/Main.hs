{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Lazy as Mp
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Snap.Core
import Snap.Http.Server (quickHttpServe)
import qualified Snap.Test as Snap.Test
import Test.Hspec

import Todo
import MySnap (MySnap, runMyHandler, getConn, runMySnap)

main :: IO ()
main = quickHttpServe . runMySnap $ mySite

------------
-- routes --

-- GET    /todos
-- GET    /todos/:id
-- POST   /todos
-- PUT    /todos/:id
-- DELETE /todos/:id

mySite :: MySnap ()
mySite =
  dir "todos" $ do
    modifyResponse (setContentType "application/json")
    route [ ("",    method GET    cIndex   >>= serialize)
          , (":id", method GET    cShow    >>= serialize)
          , ("",    method POST   cCreate  >>= serialize)
          , (":id", method PUT    cUpdate  >>= serialize)
          , (":id", method DELETE cDestroy >>= serialize) ]

serialize :: ToJSON a => a -> MySnap ()
serialize = writeBS . BSL.toStrict . encode

-------------------------
-- controller acitions --

cIndex :: MySnap [Todo]
cIndex = do
  liftIO . index =<< getConn

cShow :: MySnap Todo
cShow = do
  kId <- getDecimalParam "id"
  liftIO . flip find kId =<< getConn

cCreate :: MySnap Todo
cCreate = do
  mybJson <- decode <$> readRequestBody (2^(10::Int))
  ($ mybJson) $ maybe pass $ \todo ->
    liftIO . flip save todo =<< getConn

cUpdate :: MySnap Todo
cUpdate = do
  kId <- getDecimalParam "id"
  mybJson <- decode <$> readRequestBody (2^(10::Int))
  ($ mybJson) $ maybe pass $ \(Todo _ n c) -> do
    todo <- liftIO . flip find kId =<< getConn
    liftIO . flip save (todo {note = n, completed = c}) =<< getConn

cDestroy :: MySnap Todo
cDestroy = do
  kId <- getDecimalParam "id"
  liftIO . flip delete kId =<< getConn

-- helpers --

getDecimalParam :: (Integral a, MonadSnap m) => BS.ByteString -> m a
getDecimalParam p = do
  mybStr <- getParam p
  ($ mybStr) $ maybe pass $ \str -> do
     ($ TR.decimal (TE.decodeUtf8 str)) $ either (const pass) (return . fst)


----------
-- spec --

spec_mySite :: Spec
spec_mySite = do
  describe "GET /todos" $ it "." $ do
    let req = Snap.Test.get "/todos" Mp.empty
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "GET /todos/:id" $ it "." $ do
    let req = Snap.Test.get "/todos/11" Mp.empty
    resp <- runMyHandler req mySite
    let todo = Todo (Just 11) "john snow is dead" True
    Snap.Test.getResponseBody resp >>= (`shouldBe` BSL.toStrict (encode todo))
  describe "POST /todos" $ it "." $ do
    let todo = Todo Nothing "john snow is alive" True
    let req = Snap.Test.postRaw "/todos" "application/json" (BSL.toStrict (encode todo))
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "PUT /todos/:id" $ it "." $ do
    let todo = Todo Nothing "yyyy" True
    let req = Snap.Test.put "/todos/12" "application/json" (BSL.toStrict (encode todo))
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "DELETE /todos/:id" $ it "." $ do
    let req = Snap.Test.delete "/todos/13" Mp.empty
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
