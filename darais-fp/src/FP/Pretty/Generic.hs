module FP.Pretty.Generic where

import Data.Text (Text)
import FP.Classes.Monad
import FP.Data.DumbLattice
import FP.Classes.Sequence
import FP.Data.Bool
import Data.Char
import FP.Classes.Functor
import FP.Data.Lens
import FP.Data.List
import FP.PrePrelude
import FP.Pretty.StateSpace
import FP.Util.ConsoleState
import Prelude ()
import System.Console.ANSI
import qualified Data.Text as T

----- "Primitives" -----

rawText :: (MonadPretty m) => Text -> m ()
rawText = tellView id

rawString :: (MonadPretty m) => String -> m ()
rawString = rawText . T.pack

text :: (MonadPretty m) => Text -> m ()
text s = do
  rawText s
  modifyView columnL $ (+) $ fromIntegral $ T.length s
  modifyView ribbonL $ (+) $ countNonSpace $ T.unpack s
  m <- askView failureL
  when (m == Fail) $ do
    w <- askView layoutWidthL
    rw <- askView ribbonWidthL
    c <- getView columnL
    r <- getView ribbonL
    when (c > w) mzero
    when (r > rw) mzero
  where
    countNonSpace = siter (cond isSpace id succ) 0

string :: (MonadPretty m) => String -> m ()
string = text . T.pack

space :: (MonadPretty m) => Int -> m ()
space = text . flip T.replicate " " . fromIntegral

tryFlat :: (MonadPretty m) => m a -> m a -> m a
tryFlat dFlat dBreak = do
  l <- askView layoutL
  case l of
    Flat -> dFlat
    Break -> dBreak

hardLine :: (MonadPretty m) => m ()
hardLine = do
  i <- askView nestingL
  rawText "\n"
  putView columnL 0
  putView ribbonL 0
  space i

flat :: (MonadPretty m) => m a -> m a
flat = localViewSet layoutL Flat

flatFail :: (MonadPretty m) => m a -> m a
flatFail = localViewSet failureL Fail . flat

nest :: (MonadPretty m) => Int -> m a -> m a
nest i = localViewMod nestingL $ (+) i

group :: (MonadPretty m) => m a -> m a
group aM = tryFlat aM $ mplus (flatFail aM) aM

align :: (MonadPretty m) => m a -> m a
align aM = do
  i <- askView nestingL
  c <- getView columnL
  nest (c-i) aM

hang :: (MonadPretty m) => Int -> m a -> m a
hang i = align . nest i

----- Helpers -----

buffer :: (MonadPretty m) => m a -> m a
buffer = localViewSet (bufferingL . styleOptionsL) Buffer

noBuffer :: (MonadPretty m) => m a -> m a
noBuffer = localViewSet (bufferingL . styleOptionsL) NoBuffer

console :: (MonadPretty m) => m a -> m a
console = localViewSet doConsoleL True

noConsole :: (MonadPretty m) => m a -> m a
noConsole = localViewSet doConsoleL False

getBuff :: (MonadPretty m) => m Text
getBuff = do
  b <- askView $ bufferingL . styleOptionsL
  return $ case b of
    Buffer -> " "
    NoBuffer -> mempty

----- Style helpers -----

dropIndent :: (MonadPretty m) => m () -> m ()
dropIndent d = do
  i <- askView $ indentWidthL . styleOptionsL
  buff <- getBuff
  tryFlat (text buff) $ do
    hardLine
    space i
  align d

encloseSepPre :: (MonadPretty m) => Text -> Text -> Text -> Bool -> [m ()] -> m ()
encloseSepPre lbrac rbrac sep snug ds = 
  let lbracL = fromIntegral $ T.length lbrac
      sepL = fromIntegral $ T.length sep
  in do
    buff <- getBuff
    let f = foldr (.) id
          [ mapFirst $ \ d -> do
              punctuation $ text lbrac
              tryFlat (text buff) $ do 
                space $ sepL - lbracL
                text buff
              d
          , mapRest $ \ d -> do
              tryFlat (text buff) $ do
                hardLine
                space $ lbracL - sepL
              punctuation $ text sep
              text buff
              d
          , mapLast $ \ d -> do
              d
              if snug then text buff else tryFlat (text buff) hardLine
              punctuation $ text rbrac
          ]
    group . sequence_ . f $ map align ds

encloseSepPost :: (MonadPretty m) => Text -> Text -> Text -> [m ()] -> m ()
encloseSepPost lbrac rbrac sep ds =
  let lbracL = fromIntegral $ T.length lbrac
  in do
    buff <- getBuff
    let f = foldr (.) id $
          [ mapFirst $ \ d -> do
              punctuation $ text lbrac
              text buff
              d
          , mapRest $ \ d -> do
              tryFlat (return ()) $ do
                hardLine
                space lbracL
              text buff
              d
          , mapLeading $ \ d -> do
              d
              text buff
              punctuation $ text sep
          , mapLast $ \ d -> do
              d
              text buff
              punctuation $ text rbrac
          ]
    group . sequence_ . f $ map align ds

encloseSepIndent :: (MonadPretty m) => Text -> Text -> Text -> [m ()] -> m ()
encloseSepIndent lbrac rbrac sep ds = do
  buff <- getBuff
  i <- askView $ indentWidthL . styleOptionsL
  let f = foldr (.) id $
        [ mapFirst $ \ d -> do
            punctuation $ text lbrac
            d
        , map $ \ d -> do
            tryFlat (text buff) $ do
              hardLine
              space i
            d
        , mapLeading $ \ d -> do
            d
            text buff
            punctuation $ text sep
        , mapLast $ \ d -> do
            d
            tryFlat (text buff) hardLine
            punctuation $ text rbrac
        ]
  group $ sequence_ $ f $ map align ds

encloseSep :: (MonadPretty m) 
           => Text -> Text -> Text -> [m ()] -> m ()
encloseSep lbrac rbrac _ [] = punctuation $ text lbrac >> text rbrac
encloseSep lbrac rbrac sep ds = do
  s <- askView $ styleL . styleOptionsL
  case s of
    PreAlignStyle -> encloseSepPre lbrac rbrac sep False ds
    PreSnugStyle -> encloseSepPre lbrac rbrac sep True ds
    PostStyle -> encloseSepPost lbrac rbrac sep ds
    IndentStyle -> encloseSepIndent lbrac rbrac sep ds

encloseSepDropIndent :: (MonadPretty m) => Text -> Text -> Text -> [m ()] -> m ()
encloseSepDropIndent lbrac rbrac _ [] = punctuation $ text lbrac >> text rbrac
encloseSepDropIndent lbrac rbrac sep ds = do
  s <- askView $ styleL . styleOptionsL
  case s of
    PreAlignStyle -> dropIndent $ encloseSepPre lbrac rbrac sep False ds
    PreSnugStyle -> dropIndent $ encloseSepPre lbrac rbrac sep True ds
    PostStyle -> dropIndent $ encloseSepPost lbrac rbrac sep ds
    IndentStyle -> encloseSepIndent lbrac rbrac sep ds

hsep :: (MonadPretty m) => [m ()] -> m ()
hsep ds = do
  buff <- getBuff
  sintersperseExecute (text buff) ds

vsep :: (MonadPretty m) => [m ()] -> m ()
vsep ds = do
  buff <- getBuff
  sintersperseExecute (tryFlat (text buff) hardLine) ds

sexpListCons :: (MonadPretty m) => [m ()] -> Maybe (m ()) -> m ()
sexpListCons ds dM = group $ parens $ do
  buffer $ vsep $ ds
  case dM of
    Nothing -> return ()
    Just d -> do
      tryFlat (space 1) hardLine
      punctuation $ text ". "
      d

sexpList :: (MonadPretty m) => [m ()] -> m ()
sexpList = flip sexpListCons Nothing

flatFillTo :: (MonadPretty m) => Int -> m () -> m ()
flatFillTo i m = do
  c <- getView columnL 
  flat m
  c' <- getView columnL
  space $ max 0 $ i - (c' - c)

flatFillToR :: (MonadPretty m) => Int -> m () -> m ()
flatFillToR i m = do
  c <- getView columnL
  delta <- censor (const mempty) $ do
    flat m
    c' <- getView columnL
    return $ max 0 $ i - (c' - c)
  putView columnL c
  space delta
  flat m

topLevel :: (MonadPretty m) => m a -> m a
topLevel aM = do
  doConsole <- askView doConsoleL
  when doConsole emitConsoleStateCodes
  group aM

----- ANSI Console helpers -----

emitConsoleStateCodes :: (MonadPretty m) => m ()
emitConsoleStateCodes = do
  proceed <- askView doConsoleL
  when proceed $ do
    cs <- askView consoleStateL
    rawString $ setConsoleStateCodes cs

localConsole :: (MonadPretty m) => (ConsoleState -> ConsoleState) -> m a -> m a
localConsole f aM = do
  a <- local (modL (consoleStateL . view) f) $ do
    emitConsoleStateCodes
    aM
  emitConsoleStateCodes
  return a

intensity :: (MonadPretty m) => ConsoleIntensity -> m a -> m a
intensity = localConsole . setL intensityML . Just

italicized :: (MonadPretty m) => Bool -> m a -> m a
italicized = localConsole . setL italicizedML . Just

underlining :: (MonadPretty m) => Underlining -> m a -> m a
underlining = localConsole . setL underliningML . Just

blinkSpeed :: (MonadPretty m) => BlinkSpeed -> m a -> m a
blinkSpeed = localConsole . setL blinkSpeedML . Just

visible :: (MonadPretty m) => Bool -> m a -> m a
visible = localConsole . setL visibleML . Just

swapFgBg :: (MonadPretty m) => Bool -> m a -> m a
swapFgBg = localConsole . setL swapFgBgML . Just

gcolor :: (MonadPretty m) => ConsoleLayer -> ColorIntensity -> Color -> m a -> m a
gcolor cl ci c = localConsole $ setL gcolorML $ Just (cl,ci,c)

color :: (MonadPretty m) => ColorIntensity -> Color -> m a -> m a
color = gcolor Foreground

localStyle :: (MonadPretty m) => Lens Palette ConsoleState -> m a -> m a
localStyle l aM = do
  c <- askView $ l . paletteL
  localConsole (mappend c) aM

punctuation :: (MonadPretty m) => m a -> m a
punctuation = localStyle punctuationColorL . noConsole

literal :: (MonadPretty m) => m a -> m a
literal = localStyle literalColorL . noConsole

binder :: (MonadPretty m) => m a -> m a
binder = localStyle binderColorL . noConsole

keyword :: (MonadPretty m) => m a -> m a
keyword = localStyle keywordColorL . noConsole

classifier :: (MonadPretty m) => m a -> m a
classifier = localStyle classifierColorL . noConsole

---------- Precedence ----------

lteM :: (MonadPretty m) => Level -> m Bool
lteM l = do
  dl <- askView precDL
  cl <- askView precLevel
  return $ dlLte dl l cl

atLevel :: (MonadPretty m) => Level -> m a -> m a
atLevel = localViewSet precLevel

withLattice :: (MonadPretty m) => DumbLattice -> m a -> m a
withLattice = localViewSet precDL

parens :: (MonadPretty m) => m () -> m ()
parens aM = do
  punctuation $ string "("
  atLevel TopLevel aM
  punctuation $ string ")"

guardLevel :: (MonadPretty m) => Level -> m () -> m ()
guardLevel l aM = do
  b <- lteM l
  let f = if b then id else parens
  f $ atLevel l aM

bump :: (MonadPretty m) => m a -> m a
bump = localViewMod precLevel bumpLevel

----- Testing -----

styleVariants :: (MonadPretty m) => m () -> m ()
styleVariants aM = do
  i <- askView $ indentWidthL . styleOptionsL
  let configs =
        [ StyleOptions PreAlignStyle Buffer   i
        , StyleOptions PreAlignStyle NoBuffer i
        , StyleOptions PreSnugStyle  Buffer   i
        , StyleOptions PreSnugStyle  NoBuffer i
        , StyleOptions PostStyle     Buffer   i
        , StyleOptions PostStyle     NoBuffer i
        , StyleOptions IndentStyle   Buffer   i
        , StyleOptions IndentStyle   NoBuffer i
        ]
  forM_ configs $ \ o -> do
    hardLine
    text "##### "
    string $ show o
    text " #####"
    hardLine
    local (setL (styleOptionsL . view) o) aM
    hardLine
