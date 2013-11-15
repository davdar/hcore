{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ML.Gen where

import ML.MonadGen
import Control.Monad.Reader
import System.Random.MWC
import Control.Monad.State
import Control.Monad.Writer

newtype MGenT m a = MGenT { runMGenT :: ReaderT GenIO m a }
  deriving (Monad, MonadTrans, MonadState s, MonadWriter o, MonadIO)

instance (MonadIO m) => MonadGen (MGenT m) where
  askGen = MGenT $ ask
  localGen f = MGenT . local f . runMGenT

execMGenT :: (MonadIO m) => MGenT m a -> m a
execMGenT aM = do
  g <- liftIO createSystemRandom
  flip runReaderT g $ runMGenT aM

type MGen = MGenT IO

execMGen :: MGen a -> IO a
execMGen = execMGenT
