{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Pretty.Concrete where

import Data.Lens
import System.Process
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Pretty.Class
import Classes.HasLens
import Control.Monad.RWS
import Data.Maybe
import FPUtil.Monad
import Text.Pretty.StateSpace
import Data.Text.Lazy (Text)
import FPUtil.ConsoleState
import Text.Pretty.Generic

newtype CPretty a = CPretty 
  { unCPretty :: RWST PrettyEnv Text PrettyState Maybe a }
  deriving 
  ( Monad
  , Functor
  , MonadPlus
  , MonadReader PrettyEnv
  , MonadWriter Text
  , MonadState PrettyState
  )

type instance MEnv CPretty = PrettyEnv
type instance MOut CPretty = Text
type instance MState CPretty = PrettyState

runCPretty :: PrettyEnv -> CPretty a -> Maybe (a,PrettyState,Text)
runCPretty r aM = runRWST (unCPretty aM) r defaultPrettyState

execCPretty :: PrettyEnv -> CPretty () -> Text
execCPretty env aM =
  let aM' = do
        console <- askView doConsoleL
        when console emitConsoleStateCodes
        group aM
      ((),_,t) = fromJust $ runCPretty env aM'
  in t

show' :: (Pretty a) => a -> String
show' = T.unpack . execCPretty showPrettyEnv . pretty

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
