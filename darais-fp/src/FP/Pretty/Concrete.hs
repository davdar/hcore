module FP.Pretty.Concrete where

import Prelude ()
import FP.PrePrelude
import FP.Classes.Monad
import FP.Pretty.StateSpace
import qualified Data.Text as T
import Data.Text (Text)
import FP.Pretty.Generic

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

checkMonadPretty_CPrettyT :: (Monad m) => CPrettyT m ()
checkMonadPretty_CPrettyT = checkMonadPretty

runCPrettyT :: CPrettyT m a -> m (Maybe (a,PrettyState,Text))
runCPrettyT aM = runMaybeT $ runRWST (unCPrettyT aM) defaultPrettyEnv defaultPrettyState

execCPrettyT :: (Monad m) => CPrettyT m () -> m Text
execCPrettyT aM = do
  ((),_,t) <- liftM fromJust $ runCPrettyT aM
  return t

type CPretty = CPrettyT Identity

runCPretty :: CPretty a -> Maybe (a, PrettyState, Text)
runCPretty = runIdentity . runCPrettyT

execCPretty :: CPretty () -> Text
execCPretty = runIdentity . execCPrettyT

showPretty :: CPretty () -> String
showPretty = T.unpack . execCPretty . topLevel
