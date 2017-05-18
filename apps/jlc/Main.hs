module Main ( main ) where

import Control.Monad
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Exception

import Javalette.Syntax
import qualified Javalette.Parser as Parser
import qualified Javalette.TypeChecking as TypeChecking
import qualified Javalette.Interpreter as Interpreter
import Javalette.TypeChecking ( TypeCheck , TypeCheckingError )
import Javalette.PrettyPrint hiding ((<>))
import qualified Javalette.Compiler as Compiler
  ( runBackends
  )
import qualified Javalette.Options as StdOpts
import qualified Javalette.Backend.LLVM as LLVM

-- | Runs the compiler on all files given as arguments.
main :: IO ()
main = compile

-- | Either a parse error or a typechecking error.
data CompilerErr = ParseErr String | TypeErr TypeCheckingError deriving (Show)

instance Pretty CompilerErr where
  pPrint err = case err of
    ParseErr err' -> text "PARSE ERROR:" <+> pPrint err'
    TypeErr err'  -> text "TYPE ERROR:" <+> pPrint err'

-- | Parses and typechecks a program.
compile :: IO ()
compile = Compiler.runBackends $ \x -> handleErrs (parseProgram >=> typecheck $ x)

-- | Errors/success should be reported by printing to stderr.
handleErrs :: IO a -> IO a
handleErrs act = act <* ok `catch` \e -> bad >> throw (e :: SomeException)
  where
    ok = putStrLnErr "OK"
    bad = putStrLnErr "ERR"

-- | Wraps the error returned by `TypeChecking.typecheck`.
typecheck :: Prog -> IO Prog
typecheck = liftEither . TypeChecking.typecheck

-- | Wraps the error returned by `Parser.parse`.
parseProgram :: String -> IO Prog
parseProgram = liftEither . Parser.parse

liftEither :: Exception e => Either e a -> IO a
liftEither (Left err) = putStrLnErr "ERROR" >> throwIO err
liftEither (Right a)  = return a

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
