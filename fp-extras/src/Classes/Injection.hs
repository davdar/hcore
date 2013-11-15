{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Classes.Injection where

-- import Data.Monoid
-- import Control.Monad.Writer
-- import Control.Monad.State
-- import Control.Monad.TFBridge
-- 
-- class Injection b c where
--   inject :: b -> c
-- 
-- type MonadWriterInjection b m = (MonadWriterTF m, Injection b (MOut m))
-- 
-- tellInject :: (MonadWriterInjection c m) => (b -> c) -> b -> m ()
-- tellInject f = tell . inject . f
-- 
-- type MonadStateInjection b m = (MonadStateTF m, Injection b (MState m))
-- 
-- putInject :: (MonadStateInjection c m) => (b -> c) -> b -> m ()
-- putInject f = put . inject . f
-- 
-- instance Injection a a where
--   inject = id
-- 
-- instance (Monoid a) => Injection a (Endo a) where
--   inject = Endo . mappend
