module Javalette.Options
  ( Args(..)
  , StdArgs
  , parseArgsAdditional
  , argsParser
  ) where

import Options.Applicative
import Data.Semigroup

-- TODO: We might like to do something more fancy (e.g. using
-- optparse-applicative)
-- | Assumes that all argumens are paths to files. Reads the contents of these
-- files.
parseArgsAdditional :: Parser a -> ParserInfo (Args a)
parseArgsAdditional additional = info (argsParser additional <**> helper)
      ( fullDesc
      <> progDesc "Compile javalette programs"
      <> header "jlc"
      )

type StdArgs = Args ()

data Args a = Args
  { argsFilePaths :: [FilePath]
  , argsBackend   :: [String]
  , argsAdditionalArguments :: a
  }

argsParser :: Parser a -> Parser (Args a)
argsParser additional = Args
  <$> many (argument str (metavar "FILE"))
  <*> many (strOption
    ( long "backend"
    <> short 'b'
    <> metavar "BACKEND"
    <> help
      ( unlines
        [ "[THIS OPTION IS CURRENTLY IGNORED] "
        , "Only invoke BACKEND "
        , "(all backends are invoked per default for compatibility reasons)"
        ]
      )
    ))
  <*> additional
