{-# LANGUAGE OverloadedStrings #-}
module MySite (mySite, spec_mySite) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import qualified Data.Map.Lazy as Mp
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Snap.Core
import qualified Snap.Test as Snap.Test
import Test.Hspec

import Todo
import MySnap (MySnap, runMyHandler, getConn)
import Db (pgConn)

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

showBS :: Show a => a -> BS.ByteString
showBS = TE.encodeUtf8 . T.pack . show


----------
-- spec --

spec_mySite :: Spec
spec_mySite = do
  describe "GET /todos" $ it "." $ do
    let req = Snap.Test.get "/todos" Mp.empty
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "GET /todos/:id" $ it "." $ do
    conn <- pgConn
    todo <- save conn (Todo Nothing "xxxx" False)
    let req = Snap.Test.get ("/todos/" `BS.append` (showBS . fromJust . key_id $ todo)) Mp.empty
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "POST /todos" $ it "." $ do
    let todo = Todo Nothing "john snow is alive" True
    let req = Snap.Test.postRaw "/todos" "application/json" (BSL.toStrict (encode todo))
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "PUT /todos/:id" $ it "." $ do
    conn <- pgConn
    todo <- save conn (Todo Nothing "xxxx" False)
    let newTodo = Todo Nothing "yyyy" True
    let req = Snap.Test.put ("/todos/" `BS.append` (showBS . fromJust . key_id $ todo))
                            "application/json"
                            (BSL.toStrict (encode newTodo))
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
  describe "DELETE /todos/:id" $ it "." $ do
    conn <- pgConn
    todo <- save conn (Todo Nothing "xxxx" False)
    let req = Snap.Test.delete ("/todos/" `BS.append` (showBS . fromJust . key_id $ todo))
                               Mp.empty
    resp <- runMyHandler req mySite
    Snap.Test.assertSuccess resp
