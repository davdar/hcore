module FP.Pretty.Class
  ( module FP.Pretty.Class
  , MonadPretty
  ) where

import System.IO.Unsafe
import System.Process
import FP.Pretty.Concrete
import FP.Classes.Static
import FP.Data.BInt
import FP.Data.SInt
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import FP.Classes.Monad
import FP.Data.Lens
import Prelude ()
import FP.Classes.Functor
import FP.PrePrelude
import FP.Pretty.StateSpace
import FP.Pretty.Generic
import qualified Data.Text.IO as T

class Pretty a where
  pretty :: (MonadPretty m) => a -> m ()
  prettyDropIndent :: (MonadPretty m) => a -> m ()
  prettyDropIndent = dropIndent . pretty
  prettyList :: (MonadPretty m) => [a] -> m ()
  prettyList = 
    encloseSep "[" "]" "," 
    . map pretty
  prettyDropIndentList :: (MonadPretty m) => [a] -> m ()
  prettyDropIndentList =
    encloseSepDropIndent "[" "]" ","
    . map pretty

instance Pretty Bool where
  pretty = literal . string . show
instance Pretty Int where
  pretty = literal . string . show
instance Pretty Integer where
  pretty = literal . string . show
instance Pretty Double where
  pretty x = do
    dM <- askView maxDecimalL
    case dM of
      Nothing -> literal $ string $ show x
      Just d -> literal $ string $ printf ("%." ++ show d ++ "f") x
instance Pretty Char where
  pretty = literal . string . show
  prettyList = literal . string . ($ []) . showList
  prettyDropIndentList = dropIndent . prettyList
instance Pretty Text where
  pretty = literal . string . show

instance Pretty () where
  pretty () = punctuation $ text "()"
instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a,b) = encloseSep "(" ")" "," 
    [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pretty (a,b,c) = encloseSep "(" ")" "," 
    [pretty a, pretty b, pretty c]
instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a,b,c,d) where
  pretty (a,b,c,d) = encloseSep "(" ")" ","
    [pretty a, pretty b, pretty c, pretty d]

instance (Pretty a) => Pretty [a] where
  pretty = prettyList
  prettyDropIndent = prettyDropIndentList

instance (Pretty a) => Pretty (Set a) where
  pretty = 
    encloseSep "{" "}" "," 
    . map pretty
    . Set.toList
  prettyDropIndent =
    encloseSepDropIndent "{" "}" ","
    . map pretty
    . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = 
    encloseSep "{" "}" "," 
    . map prettyMapping
    . Map.toList
  prettyDropIndent =
    encloseSepDropIndent "{" "}" ","
    . map prettyMapping
    . Map.toList

instance Pretty (SInt i) where
  pretty = pretty . stripS
instance Show (SInt i) where
  show = show'

instance Pretty (BInt i) where
  pretty = pretty . stripS
instance Show (BInt i) where
  show = show'

prettyMapping :: (MonadPretty m, Pretty k, Pretty v) => (k,v) -> m ()
prettyMapping (k,v) = group $ hsep
  [ pretty k
  , punctuation $ text "=>"
  , prettyDropIndent v
  ]

prettyFromShow :: (MonadPretty m, Show a) => a -> m ()
prettyFromShow = string . show

type PrettyF a = forall m. (MonadPretty m) => a -> m ()

show' :: (Pretty a) => a -> String
show' = showPretty . pretty

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
