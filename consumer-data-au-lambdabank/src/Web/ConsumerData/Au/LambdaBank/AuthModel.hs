{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.ConsumerData.Au.LambdaBank.AuthModel where

import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVarIO, writeTVar)
import Control.Monad.Free          (MonadFree, liftF)
import Control.Monad.Free.Church   (F, iterM)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Reader.Class  (MonadReader, ask)

data AuthModelF next where
  IncrementCount :: (Integer -> next) -> AuthModelF next

deriving instance Functor AuthModelF

incrementCount ::
  MonadFree AuthModelF m
  => m Integer
incrementCount =
  liftF $ IncrementCount id

type AuthModelM = F AuthModelF

runAuthModelM ::
  ( MonadIO m
  , MonadReader (TVar Integer) m
  )
  => AuthModelM a
  -> m a
runAuthModelM = iterM $ \case
  (IncrementCount next) -> do
    counterVar <- ask
    counter <- liftIO . fmap succ $ readTVarIO counterVar
    liftIO . atomically $ writeTVar counterVar counter
    next counter
