{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ML.Gen where

import Prelude ()
import FP
import ML.MonadGen
import System.Random.MWC

newtype MGenT m a = MGenT { runMGenT :: ReaderT GenIO m a }
  deriving (Monad, MonadTrans, MonadState s, MonadWriter o, MonadIO)

type instance MEnv (MGenT m) = MEnv m
type instance MOut (MGenT m) = MOut m
type instance MState (MGenT m) = MState m

instance (MonadReader r m) => MonadReader r (MGenT m) where
  ask = lift ask
  local f = MGenT . ReaderT . ((.) $ local f) . runReaderT . runMGenT

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
