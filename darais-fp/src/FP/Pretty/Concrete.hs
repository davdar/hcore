module FP.Pretty.Concrete where

import Prelude ()
import FP.PrePrelude
import Data.Maybe
import FP.Data.Lens
import FP.Classes.Monad
import FP.Pretty.StateSpace
import FP.Data.Function
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

runCPrettyT :: PrettyEnv -> CPrettyT m a -> m (Maybe (a,PrettyState,Text))
runCPrettyT r aM = runMaybeT $ runRWST (unCPrettyT aM) r defaultPrettyState

execCPrettyT :: (Monad m) => PrettyEnv -> CPrettyT m () -> m Text
execCPrettyT env aM = do
  let aM' = do
        doConsole <- askView doConsoleL
        when doConsole emitConsoleStateCodes
        group aM
  ((),_,t) <- liftM fromJust $ runCPrettyT env aM'
  return t

type CPretty = CPrettyT Identity

runCPretty :: PrettyEnv -> CPretty a -> Maybe (a, PrettyState, Text)
runCPretty = runIdentity .: runCPrettyT

execCPretty :: PrettyEnv -> CPretty () -> Text
execCPretty = runIdentity .: execCPrettyT


showPretty :: CPretty () -> String
showPretty = T.unpack . execCPretty showPrettyEnv

