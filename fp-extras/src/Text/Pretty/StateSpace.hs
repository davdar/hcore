{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.Pretty.StateSpace where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.State
import Control.Monad.Writer
import FPUtil.Monad
import Control.Monad.Reader
import Data.Function hiding (id, (.))
import Classes.PartialOrder
import Data.List
import System.Console.ANSI
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import FPUtil.ConsoleState
import Data.Lens.Template
import Data.Lens
import Classes.HasLens

data Layout = Flat | Break
  deriving (Eq, Ord, Show, Enum)
data Failure = Fail | NoFail
  deriving (Eq, Ord, Show, Enum)

data Style = PreAlignStyle | PreSnugStyle | PostStyle | IndentStyle
  deriving (Eq, Ord, Show, Enum)
data Buffering = Buffer | NoBuffer
  deriving (Eq, Ord, Show, Enum)

data Direction = NoD | LeftD | RightD
  deriving (Eq, Show, Enum)

instance PartialOrder Direction where
  lte NoD _ = True
  lte _ NoD = False
  lte LeftD LeftD = True
  lte LeftD RightD = False
  lte RightD LeftD = False
  lte RightD RightD = True
  
data Precedence = Precedence Int Direction Bool
  deriving (Eq, Show)

instance PartialOrder Precedence where
  pcompare = pcompare `on` toAlg
    where
    toAlg (Precedence n d b) = ((n,d),b)

pbump :: Precedence -> Precedence
pbump (Precedence n k b) = Precedence n k True

data StyleOptions = StyleOptions
  { _styleL :: Style
  , _bufferingL :: Buffering
  , _indentWidthL :: Int
  } deriving (Eq, Ord, Show)
makeLens ''StyleOptions

defaultPreOptions :: StyleOptions
defaultPreOptions = StyleOptions PreAlignStyle Buffer 2

defaultPostOptions :: StyleOptions
defaultPostOptions = StyleOptions PostStyle NoBuffer 2

defaultIndentStyle :: StyleOptions
defaultIndentStyle = StyleOptions IndentStyle NoBuffer 2

data Palette = Palette
  { _punctuationColorL :: ConsoleState
  , _literalColorL :: ConsoleState
  , _binderColorL :: ConsoleState
  , _keywordColorL :: ConsoleState
  , _classifierColorL :: ConsoleState
  } deriving (Eq, Ord, Show)
makeLens ''Palette

defaultPalette :: Palette
defaultPalette = Palette
  { _punctuationColorL = setConsoleColor Dull Yellow
  , _literalColorL = setConsoleColor Dull Red
  , _binderColorL = setConsoleColor Dull Cyan
  , _keywordColorL = 
      setConsole underliningML SingleUnderline
      `mappend` setConsole intensityML BoldIntensity
  , _classifierColorL = setConsoleColor Dull Magenta
  }

data PrettyEnv = PrettyEnv
  -- layout options
  { _layoutWidthL :: Int
  , _ribbonRatioL :: Double
  -- dynamic environment
  , _nestingL :: Int
  , _layoutL :: Layout
  , _failureL :: Failure
  -- , _depth :: Int
  , _precedenceL :: (Precedence,Precedence)
  -- style
  , _styleOptionsL :: StyleOptions
  -- truncation
  -- , _truncateDepth :: Int
  -- , _truncate :: Bool
  -- console
  , _paletteL :: Palette
  , _consoleStateL :: ConsoleState
  , _doConsoleL :: Bool
  , _maxDecimalL :: Maybe Int
  } deriving (Eq, Show)
makeLens ''PrettyEnv

defaultPrettyEnv :: PrettyEnv
defaultPrettyEnv = PrettyEnv
  { _layoutWidthL = 80
  , _ribbonRatioL = 0.8
  , _nestingL = 0
  , _layoutL = Break
  , _failureL = NoFail
  , _precedenceL = (Precedence 0 NoD False,Precedence 0 NoD False)
  , _styleOptionsL = defaultPreOptions
  , _paletteL = defaultPalette
  , _consoleStateL = emptyConsoleState
  , _doConsoleL = True
  , _maxDecimalL = Nothing
  }

noConsolePrettyEnv :: PrettyEnv
noConsolePrettyEnv =
  setL doConsoleL False defaultPrettyEnv

showPrettyEnv :: PrettyEnv
showPrettyEnv =
  setL (bufferingL . styleOptionsL) NoBuffer $
  setL layoutL Flat $
  noConsolePrettyEnv
    
data PrettyState = PrettyState
  { _columnL :: Int
  , _ribbonL :: Int
  } deriving (Eq, Ord, Show)
makeLens ''PrettyState

defaultPrettyState :: PrettyState
defaultPrettyState = PrettyState
  { _columnL = 0 
  , _ribbonL = 0
  }

type (MonadPretty m) = (MonadRWSView PrettyEnv Text PrettyState m, MonadPlus m)
