module Main ( main ) where

import Control.Monad
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Options.Applicative
import Data.Semigroup ((<>))

import Javalette.Syntax
import qualified Javalette.Parser as Parser
import qualified Javalette.TypeChecking as TypeChecking
import qualified Javalette.Interpreter as Interpreter
import Javalette.TypeChecking ( TypeCheck , TypeCheckingError )
import Javalette.PrettyPrint hiding ((<>))
import qualified Javalette.Compiler as Compiler (execAllBackends)

-- | Runs the compiler on all files given as arguments.
main :: IO ()
main = do
  inp <- parseInput
  mapM_ compile (argsFilePaths inp)

handleErrors :: Pretty err => Either err t -> IO t
handleErrors errOrT = case errOrT of
    Left err  -> do
      putStrLnErr "ERROR"
      prettyPrint err
      exitFailure
    Right t -> do
      putStrLnErr "OK"
      return t

-- TODO: We might like to do something more fancy (e.g. using
-- optparse-applicative)
-- | Assumes that all argumens are paths to files. Reads the contents of these
-- files.
parseInput :: IO (Args ())
parseInput = execParser opts
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
      <> progDesc "Compile javalette programs"
      <> header "jlc"
      )

data Args a = Args
  { argsFilePaths :: [FilePath]
  , argsBackend   :: [String]
  , argsBackendArguments :: a
  }

argsParser :: Parser (Args ())
argsParser = Args
  <$> many (argument str (metavar "FILE"))
  <*> many (strOption
    ( long "backend"
    <> short 'b'
    <> metavar "BACKEND"
    <> help
      ( unlines
        [ "Only invoke BACKEND "
        , "(all backends are invoked per default for compatibility reasons)"
        ]
      )
    ))
  <*> pure ()

-- | Either a parse error or a typechecking error.
data CompilerErr = ParseErr String | TypeErr TypeCheckingError deriving (Show)

instance Pretty CompilerErr where
  pPrint err = case err of
    ParseErr err' -> text "PARSE ERROR:" <+> pPrint err'
    TypeErr err'  -> text "TYPE ERROR:" <+> pPrint err'

-- | Parses and typechecks a program.
compile :: FilePath -> IO ()
compile fp = do
  s <- readFile fp
  pAnnt <- handleErrors $ parseProgram s >>= typecheck
  Compiler.execAllBackends (dropExtension fp) pAnnt

-- | Wraps the error returned by `TypeChecking.typecheck`.
typecheck :: Prog -> Either CompilerErr Prog
typecheck = inLeft TypeErr . TypeChecking.typecheck

-- | Wraps the error returned by `Parser.parse`.
parseProgram :: String -> Either CompilerErr Prog
parseProgram = inLeft ParseErr . Parser.parse

-- | Prints to stderr.
putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

-- I would like to generalize `typecheck` to return an arbitrary `MonadError`
-- instance. And then I would like to replace `inLeft` with a function like
-- this one:
--
-- > withExcept :: (MonadError e m, MonadError e' m) => (e -> e') -> m a -> m a
-- > withExcept f m = m `catchError` (throwError . f)
--
-- But this causes a problem with some functional dependencies which I don't
-- fully understand.
inLeft :: (a -> c) -> Either a b -> Either c b
inLeft f (Left a)    = Left (f a)
inLeft _ (Right a)   = Right a
