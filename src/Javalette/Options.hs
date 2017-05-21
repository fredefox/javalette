module Javalette.Options
  ( StdArgs(..)
  , argsParser
  , programInfo
  ) where

import Options.Applicative
import Data.Semigroup

data StdArgs = Args
  { argsFilePaths :: [FilePath]
  }

argsParser :: Parser StdArgs
argsParser = Args
  <$> many (argument str (metavar "FILE"))

programInfo :: InfoMod a
programInfo
  = fullDesc
  <> progDesc "Compile javalette programs"
  <> header "jlc"
