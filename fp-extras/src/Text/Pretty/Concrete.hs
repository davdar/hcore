{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Pretty.Concrete where

import System.IO.Unsafe
import FPUtil.Function
import Classes.HasLens
import Control.Monad.RWS
import Data.Lens.Common
import Data.Maybe
import Data.Text.Lazy (Text)
import FPUtil.ConsoleState
import FPUtil.Monad
import System.Process
import Text.Pretty.Class
import Text.Pretty.Generic
import Text.Pretty.StateSpace
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad.Maybe
import Control.Monad.Identity

newtype CPrettyT m a = CPrettyT
  { unCPrettyT :: RWST PrettyEnv Text PrettyState (MaybeT m) a }
  deriving 
  ( Monad
  , Functor
  , MonadPlus
  , MonadReader PrettyEnv
  , MonadWriter Text
  , MonadState PrettyState
  )

type instance MEnv (CPrettyT m) = PrettyEnv
type instance MOut (CPrettyT m) = Text
type instance MState (CPrettyT m) = PrettyState

runCPrettyT :: PrettyEnv -> CPrettyT m a -> m (Maybe (a,PrettyState,Text))
runCPrettyT r aM = runMaybeT $ runRWST (unCPrettyT aM) r defaultPrettyState

execCPrettyT :: (Monad m) => PrettyEnv -> CPrettyT m () -> m Text
execCPrettyT env aM = do
  let aM' = do
        console <- askView doConsoleL
        when console emitConsoleStateCodes
        group aM
  ((),_,t) <- liftM fromJust $ runCPrettyT env aM'
  return t

type CPretty = CPrettyT Identity

runCPretty :: PrettyEnv -> CPretty a -> Maybe (a, PrettyState, Text)
runCPretty = runIdentity .: runCPrettyT

execCPretty :: PrettyEnv -> CPretty () -> Text
execCPretty = runIdentity .: execCPrettyT

show' :: (Pretty a) => a -> String
show' = showPretty . pretty



showPretty :: CPretty () -> String
showPretty = T.unpack . execCPretty showPrettyEnv

pprintWith :: (Pretty a) => PrettyEnv -> a -> IO ()
pprintWith e x = do
  c <- liftM read $ readProcess "tput" ["cols"] ""
  T.putStr $ execCPretty (setL layoutWidthL c e) $ pretty x

pprint :: (Pretty a) => a -> IO ()
pprint = pprintWith defaultPrettyEnv

pprintLnWith :: (Pretty a) => PrettyEnv -> a -> IO ()
pprintLnWith e x = pprintWith e x >> putStrLn ""

pprintLn :: (Pretty a) => a -> IO ()
pprintLn = pprintLnWith defaultPrettyEnv

trace' :: (Pretty a) => String -> a -> b -> b
trace' ann t x = unsafePerformIO $ do
  putStr ann
  pprintLn t
  return x
