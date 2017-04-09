module Main ( main ) where

import Control.Monad
import System.IO
import System.Environment

import Javalette.Syntax.AbsJavalette
import qualified Javalette.Parser as Parser
import qualified Javalette.TypeChecking as TypeChecking
import qualified Javalette.Interpreter as Interpreter
import Javalette.TypeChecking ( TypeCheck , TypeCheckingError )
import Javalette.PrettyPrint

-- | Runs the compiler on all files given as arguments.
main :: IO ()
main = do
  inp <- parseInput
  mapM_ (handleErrors . compile) inp

handleErrors t =case t of
    Left err  -> do
      putStrLnErr "ERROR"
      prettyPrint err
    Right{} -> putStrLnErr "OK"

-- TODO: We might like to do something more fancy (e.g. using
-- optparse-applicative)
-- | Assumes that all argumens are paths to files. Reads the contents of these
-- files.
parseInput :: IO [String]
parseInput = getArgs >>= mapM readFile

-- | Either a parse error or a typechecking error.
data CompilerErr = ParseErr String | TypeErr TypeCheckingError deriving (Show)

instance Pretty CompilerErr where
  pPrint err = case err of
    ParseErr err' -> text "PARSE ERROR:" <+> pPrint err'
    TypeErr err'  -> text "TYPE ERROR:" <+> pPrint err'

-- | Parses and typechecks a program.
compile :: String -> Either CompilerErr ()
compile s = do
  p <- parseProgram s
  typecheck p

-- | Wraps the error returned by `TypeChecking.typecheck`.
typecheck :: Prog -> Either CompilerErr ()
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
