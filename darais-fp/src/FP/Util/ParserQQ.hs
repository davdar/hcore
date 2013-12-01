module FP.Util.ParserQQ where

import Prelude ()
import FP.PrePrelude
import FP.Classes.ToQQ
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH

parserQQ :: (ToQQ a) => (String -> Either String a) -> QuasiQuoter
parserQQ parser = QuasiQuoter expQ patQ undefined undefined
  where
    expQ input = do
      locMsg <- locationError
      case parser input of
        Left e ->
          fail $ "\n" ++ locMsg ++ "\n" ++ e
        Right s ->
          toQQE s
    patQ input = do
      locMsg <- locationError
      case parser input of
        Left e ->
          fail $ "\n" ++ locMsg ++ "\n" ++ e
        Right s ->
          toQQP s
    locationError = do
      (TH.Loc fn _ _ (sl, sc) (el, ec)) <- TH.location
      return $ printf "QQ %s[%s:%s-%s:%s]" 
                      fn (show sl) (show sc) (show el) (show ec)
