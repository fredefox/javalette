module Main ( main ) where

import Control.Monad
import System.IO
import System.Environment

import Javalette.Syntax.AbsJavalette
import qualified Javalette.Parser as Parser
import qualified Javalette.TypeChecking as TypeChecking
import qualified Javalette.Interpreter as Interpreter
import Javalette.TypeChecking ( TypeCheck , TypeCheckingError )

main :: IO ()
main = do
  inp <- parseInput
  case compile inp of
    Left err  -> do
      putStrLnErr "ERROR"
      print err
    Right{} -> do
      putStrLnErr "OK"
      interp inp

t = readFile "jlctests/testsuite/core/core005.jl" >>= interp

interp p = case parseProgram p of
  Left{} -> return ()
  Right p -> Interpreter.interpret p

-- TODO: We might like to do something more fancy (e.g. using
-- optparse-applicative)
-- | The test-runner supplies a single argument that is the file to compile - so
-- we'll just read that.
parseInput :: IO String
parseInput = head <$> getArgs >>= readFile

data CompilerErr = ParseErr String | TypeErr TypeCheckingError deriving (Show)

compile :: String -> Either CompilerErr ()
compile s = do
  p <- parseProgram s
  typecheck p
  staticControlFlowCheck p

typecheck :: TypeCheck a => a -> Either CompilerErr ()
typecheck = inLeft TypeErr
  . TypeChecking.evalTypeChecker
  . TypeChecking.typecheck

staticControlFlowCheck :: Prog -> Either CompilerErr ()
staticControlFlowCheck = inLeft TypeErr
  . TypeChecking.evalTypeChecker
  . TypeChecking.staticControlFlowCheck

parseProgram :: String -> Either CompilerErr Prog
parseProgram s = inLeft ParseErr (Parser.parse s)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

inLeft :: (a -> c) -> Either a b -> Either c b
inLeft f (Left a)    = Left (f a)
inLeft _ (Right a)   = Right a
