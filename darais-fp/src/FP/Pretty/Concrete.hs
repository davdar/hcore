module FP.Pretty.Concrete where

import Prelude ()

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
