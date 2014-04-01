module FP.Pretty.Class
  ( module FP.Pretty.Class
  , MonadPretty
  ) where

import Data.Function (on)
import FP.Data.DumbLattice
import FP.Data.Proxy
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

data PrettyString = PrettyString { prettyString :: forall m. (MonadPretty m) => m () }

class Pretty a where
  precLattice :: Proxy a -> DumbLattice
  precLattice Proxy = Map.empty
  pretty :: (MonadPretty m) => a -> m ()
  prettyDropIndent :: (MonadPretty m) => a -> m ()
  prettyDropIndent = dropIndent . pretty
  prettyList :: (MonadPretty m) => [a] -> m ()
  prettyList = 
    atLevel TopLevel
    . encloseSep "[" "]" "," 
    . map pretty
  prettyDropIndentList :: (MonadPretty m) => [a] -> m ()
  prettyDropIndentList =
    atLevel TopLevel
    . encloseSepDropIndent "[" "]" ","
    . map pretty

instance Pretty PrettyString where
  pretty = prettyString
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
  precLattice Proxy =
    Map.unionsWith Set.union 
      [ precLattice (proxy :: Proxy a)
      , precLattice (proxy :: Proxy b) 
      ]
  pretty (a,b) = 
    atLevel TopLevel
    $ encloseSep "(" ")" "," 
    $ [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  precLattice Proxy =
    Map.unionsWith Set.union 
      [ precLattice (proxy :: Proxy a)
      , precLattice (proxy :: Proxy b) 
      , precLattice (proxy :: Proxy c) 
      ]
  pretty (a,b,c) = 
    atLevel TopLevel
    $ encloseSep "(" ")" ","
    $ [pretty a, pretty b, pretty c]
instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a,b,c,d) where
  precLattice Proxy =
    Map.unionsWith Set.union 
      [ precLattice (proxy :: Proxy a)
      , precLattice (proxy :: Proxy b) 
      , precLattice (proxy :: Proxy c) 
      , precLattice (proxy :: Proxy d)
      ]
  pretty (a,b,c,d) = 
    atLevel TopLevel
    $ encloseSep "(" ")" ","
    $ [pretty a, pretty b, pretty c, pretty d]

instance (Pretty a) => Pretty [a] where
  precLattice Proxy = precLattice (proxy :: Proxy a)
  pretty = prettyList
  prettyDropIndent = prettyDropIndentList

instance (Pretty a) => Pretty (Set a) where
  precLattice Proxy = precLattice (proxy :: Proxy a)
  pretty = 
    atLevel TopLevel
    . encloseSep "{" "}" "," 
    . map pretty
    . Set.toList
  prettyDropIndent =
    atLevel TopLevel
    . encloseSepDropIndent "{" "}" ","
    . map pretty
    . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  precLattice Proxy = 
    Map.unionsWith Set.union
      [ precLattice (proxy :: Proxy k)
      , precLattice (proxy :: Proxy v)
      ]
  pretty = 
    atLevel TopLevel
    . encloseSep "{" "}" "," 
    . map prettyMapping
    . Map.toList
  prettyDropIndent =
    atLevel TopLevel
    . encloseSepDropIndent "{" "}" ","
    . map prettyMapping
    . Map.toList

prettyMapping :: (MonadPretty m, Pretty k, Pretty v) => (k,v) -> m ()
prettyMapping (k,v) = group $ hsep
  [ pretty k
  , punctuation $ text "=>"
  , prettyDropIndent v
  ]

instance Pretty (SInt i) where
  pretty = pretty . stripS
instance Show (SInt i) where
  show = show'

instance Pretty (BInt i) where
  pretty = pretty . stripS
instance Show (BInt i) where
  show = show'

prettyFromShow :: (MonadPretty m, Show a) => a -> m ()
prettyFromShow = string . show

type PrettyF a = forall m. (MonadPretty m) => a -> m ()

show' :: (Pretty a) => a -> String
show' = showPretty . pretty

instance Show PrettyString where
  show = show'
instance Eq PrettyString where
  (==) = (==) `on` show'
instance Ord PrettyString where
  compare = compare `on` show'

pprintWith :: forall a. (Pretty a) => (CPretty () -> CPretty ()) -> a -> IO ()
pprintWith f x = do
  c <- liftM read $ readProcess "tput" ["cols"] ""
  T.putStr 
    $ execCPretty 
    $ localViewSet layoutWidthL c 
    $ withLattice (precLattice (proxy :: Proxy a)) 
    $ topLevel 
    $ f
    $ pretty x

pprint :: (Pretty a) => a -> IO ()
pprint = pprintWith id

pprintLnWith :: (Pretty a) => (CPretty () -> CPretty ()) -> a -> IO ()
pprintLnWith f x = pprintWith f x >> putStrLn ""

pprintLn :: (Pretty a) => a -> IO ()
pprintLn = pprintLnWith id

trace' :: (Pretty a) => String -> a -> b -> b
trace' ann t x = unsafePerformIO $ do
  putStr ann
  pprintLn t
  return x
