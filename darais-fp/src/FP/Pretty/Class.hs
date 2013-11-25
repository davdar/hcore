module FP.Pretty.Class where

import Prelude ()

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

prettyMapping :: (MonadPretty m, Pretty k, Pretty v) => (k,v) -> m ()
prettyMapping (k,v) = group $ hsep
  [ pretty k
  , punctuation $ text "=>"
  , prettyDropIndent v
  ]

prettyFromShow :: (MonadPretty m, Show a) => a -> m ()
prettyFromShow = string . show

type PrettyF a = forall m. (MonadPretty m) => a -> m ()
