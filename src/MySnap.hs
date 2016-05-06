{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MySnap (MySnap, runMyHandler, getConn) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.CatchIO
import Database.PostgreSQL.Simple
import Snap.Core
import Snap.Iteratee (run_)
import Snap.Test (runHandlerM, RequestBuilder)

-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/deriving.html
newtype MySnap a = MySnap (ReaderT Connection Snap a)
                 deriving (Functor, Applicative, Monad,
                           MonadIO, MonadCatchIO, MonadPlus, Alternative, (MonadReader Connection))

instance MonadSnap MySnap where
  liftSnap snp = MySnap $ ReaderT $ \_ -> snp

runMySnap :: MySnap a -> Snap a
runMySnap (MySnap rt) = do
  conn <- liftIO $ connect . read =<< readFile "db/config.txt"
  runReaderT rt conn <* liftIO (close conn)

runMyHandler :: MonadIO m => RequestBuilder m () -> MySnap a -> m Response
runMyHandler = runHandlerM rs
  where
    rs rq s = do
      (_, rsp) <- liftIO $ run_ $
                    (runSnap . runMySnap) s
                                          (const $ return $! ())
                                          (const $ return $! ())
                                          rq
      return rsp

getConn :: MySnap Connection
getConn = ask
