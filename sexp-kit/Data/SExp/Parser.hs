module Data.SExp.Parser where

import Prelude ()
import FP
import Data.SExp.Data
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Text.Parsec as P
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.String as P
import FPUtil.ParserQQ

sexp :: TH.QuasiQuoter
sexp = parserQQ $ runParser psexp

slist :: TH.QuasiQuoter
slist = parserQQ $ right (uncurry sexpFromList) . runParser pslist
  where
    right f (Left x) = Left x
    right f (Right x) = Right $ f x

runParser :: TokParser a -> String -> Either String a
runParser p s = mapLeft (show :: P.ParseError -> String) $ do
  ts <- P.runP tokenize () "" s
  let ts' = flip filter ts $ \pt -> 
        let t = pgetToken pt
        in not $ isJust (getWhitespaceT t) || isJust (getCommentT t)
  P.runP p () "" ts'

----- Tokenizing -----

data Token =
    LParenT
  | RParenT
  | DotT
  | WhitespaceT
  | CommentT String
  | AntiT Anti String
  | SymbolT String
  | IntegerT Integer
  | DoubleT Double
  | StringT String
  deriving (Eq, Ord, Show)

getLParenT     :: Token -> Maybe ()            ; getLParenT     t = case t of { LParenT     -> Just ()    ; _ -> Nothing }
getRParenT     :: Token -> Maybe ()            ; getRParenT     t = case t of { RParenT     -> Just ()    ; _ -> Nothing }
getDotT        :: Token -> Maybe ()            ; getDotT        t = case t of { DotT        -> Just ()    ; _ -> Nothing }
getWhitespaceT :: Token -> Maybe ()            ; getWhitespaceT t = case t of { WhitespaceT -> Just ()    ; _ -> Nothing }
getCommentT    :: Token -> Maybe String        ; getCommentT    t = case t of { CommentT s  -> Just s     ; _ -> Nothing }
getAntiT       :: Token -> Maybe (Anti,String) ; getAntiT       t = case t of { AntiT a s   -> Just (a,s) ; _ -> Nothing }
getSymbolT     :: Token -> Maybe String        ; getSymbolT     t = case t of { SymbolT s   -> Just s     ; _ -> Nothing }
getIntegerT    :: Token -> Maybe Integer       ; getIntegerT    t = case t of { IntegerT i  -> Just i     ; _ -> Nothing }
getDoubleT     :: Token -> Maybe Double        ; getDoubleT     t = case t of { DoubleT d   -> Just d     ; _ -> Nothing }
getStringT     :: Token -> Maybe String        ; getStringT     t = case t of { StringT s   -> Just s     ; _ -> Nothing }

data PToken = PToken { pgetToken :: Token,  pgetText :: String, pgetSrc :: P.SourcePos }
  deriving (Eq, Ord, Show)

tokenize :: P.Parser [PToken]
tokenize = P.many token <* P.eof

token :: P.Parser PToken
token = withPosition $ msum
  [ lparen
  , rparen
  , dot
  , whitespace
  , comment
  , anti
  , string
  , number
  , symbol
  ]

withPosition :: P.Parser (Token, String) -> P.Parser PToken
withPosition tsM = do
  p <- P.getPosition
  (t, s) <- tsM
  return $ PToken t s p

lparen :: P.Parser (Token, String)
lparen = P.char '(' >> return (LParenT, "(")

rparen :: P.Parser (Token, String)
rparen = P.char ')' >> return (RParenT, ")")

dot :: P.Parser (Token, String)
dot = P.char '.' >> return (DotT, ".")

whitespace :: P.Parser (Token, String)
whitespace = do
  s <- P.many1 $ P.oneOf " \t\r\n"
  return (WhitespaceT, s)
   
comment :: P.Parser (Token, String)
comment = P.try sexpComment P.<|> P.try multilineComment P.<|> singlelineComment

sexpComment :: P.Parser (Token, String)
sexpComment = do
  P.string "#("
  s <- nestedSExpComment 1
  let s' = "#(" ++ s
  return (CommentT s', s')

nestedSExpComment :: Int -> P.Parser String
nestedSExpComment 0 = return ""
nestedSExpComment i = do
  c <- P.anyChar
  (c:) <$> case c of
    '(' -> nestedSExpComment (i+1)
    ')' -> nestedSExpComment (i-1)
    _ -> nestedSExpComment i

multilineComment :: P.Parser (Token, String)
multilineComment = do
  P.string "#|"
  s <- nestedMultilineComment 1
  let s' = "#|" ++ s
  return (CommentT s', s')

nestedMultilineComment :: Int -> P.Parser String
nestedMultilineComment 0 = return ""
nestedMultilineComment i = do
  s <- msum
    [ P.try $ P.string "#|"
    , P.try $ P.string "|#"
    , (:[]) <$> P.anyChar
    ]
  (s ++) <$> case s of
    "#|" -> nestedMultilineComment (i+1)
    "|#" -> nestedMultilineComment (i-1)
    _    -> nestedMultilineComment i

singlelineComment :: P.Parser (Token, String)
singlelineComment = do
  P.char '#'
  s <- P.many $ P.noneOf "\r\n"
  end <- P.string "\n" P.<|> P.string "\r\n"
  let s' = "#" ++ s ++ end
  return (CommentT s', s')

anti :: P.Parser (Token, String)
anti = do
  P.char '@'
  code <- many P.letter
  P.char ':'
  case lookup code antiCodes of
    Nothing -> fail "invalid anti code"
    Just a -> do
      s <- msum
        [ do
            c <- P.letter
            cs <- P.many P.alphaNum <|> P.string "'"
            return $ c:cs
        , P.string "_"
        ]
      return (AntiT a s, "@" ++ code ++ ":" ++ s)

string :: P.Parser (Token, String)
string = do
  P.char '"'
  s <- liftM concat $ P.many $ msum 
    [ P.try $ P.string "\\\"" >> return "\""
    , P.try $ P.string "\\\\" >> return "\\"
    , (:[]) <$> P.noneOf "\\\"" 
    ]
  P.char '"'
  return (StringT s, show s)

antiCodes :: [(String, Anti)]
antiCodes =
  [ (""   , ValA)
  , ("lit", LitA)
  , ("num", NumA)
  , ("int", IntA)
  , ("dbl", DblA)
  , ("str", StrA)
  , ("sym", SymA)
  ]
    
number :: P.Parser (Token, String)
number = do
  sign <- P.option "" $ P.string "-"
  msum
    [ P.try $ do
        ns <- P.many1 P.digit
        P.char '.'
        ms <- P.many1 P.digit
        let s = sign ++ ns ++ "." ++ ms
        return (DoubleT $ read s, s)
    , do
        ns <- P.many1 P.digit
        let s = sign ++ ns
        return (IntegerT $ read s, s)
    ]

symbol :: P.Parser (Token, String)
symbol = do
  s <- P.many1 $ P.noneOf illegal
  return (SymbolT s, s)
  where
    -- @ is for anti-patterns (@$x @Sym$x @Lit$x)
    -- # is for nested comments (#| ... |$)
    -- ` is quasiquote
    -- ' is quote
    -- , is unquote
    -- . is cons (a . b)
    -- ; is comment (; ... \n)
    -- " is for strings ("foo")
    illegal = " \t\r\n()[]@#`',.;\""

----- Parsing -----

type TokParser = P.GenParser PToken ()

ptoken :: (Token -> Maybe a) -> TokParser a
ptoken f = P.token pgetText pgetSrc $ f . pgetToken

plparen     :: TokParser ()            ; plparen     = ptoken getLParenT
prparen     :: TokParser ()            ; prparen     = ptoken getRParenT
pdot        :: TokParser ()            ; pdot        = ptoken getDotT
pwhitespace :: TokParser ()            ; pwhitespace = ptoken getWhitespaceT
pcomment    :: TokParser String        ; pcomment    = ptoken getCommentT
panti       :: TokParser (Anti,String) ; panti       = ptoken getAntiT
psymbol     :: TokParser String        ; psymbol     = ptoken getSymbolT
pinteger    :: TokParser Integer       ; pinteger    = ptoken getIntegerT
pdouble     :: TokParser Double        ; pdouble     = ptoken getDoubleT
pstring     :: TokParser String        ; pstring     = ptoken getStringT

psexp :: TokParser SExp
psexp = msum
  [ uncurry sexpFromList <$> P.between plparen prparen pslist
  , SymS <$> psymbol
  , LitS <$> plit
  , uncurry AntiS <$> panti
  ]

pslist :: TokParser ([SExp], Maybe SExp)
pslist = msum
  [ do
      s <- psexp
      ss <- P.many psexp
      cons <- P.option Nothing $ do
        pdot
        Just <$> psexp

      return (s:ss, cons)
  , return ([], Nothing)
  ]

plit :: TokParser Lit
plit = msum
  [ NumberL <$> pnumber
  , StringL <$> pstring
  ]

pnumber :: TokParser Number
pnumber = msum
  [ IntegerN <$> pinteger
  , DoubleN <$> pdouble
  ]
