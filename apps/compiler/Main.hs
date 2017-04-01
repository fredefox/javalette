module Main ( main ) where

import System.IO
import System.Environment

import Javalette.Syntax.AbsJavalette
import Javalette.Parser ( parse )
import qualified Javalette.TypeChecking as TypeChecking
import Javalette.TypeChecking ( TypeCheck , TypeCheckingError )

main :: IO ()
main = do
  inp <- parseInput
  case compile inp of
    Left err  -> do
      putStrLnErr "BAD"
      print err
    Right{} -> putStrLnErr "OK"

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

typecheck :: TypeCheck a => a -> Either CompilerErr ()
typecheck = inLeft TypeErr
  . TypeChecking.evalTypeChecker
  . TypeChecking.typecheck

parseProgram :: String -> Either CompilerErr Prog
parseProgram s = inLeft ParseErr (parse s)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

inLeft :: (a -> c) -> Either a b -> Either c b
inLeft f (Left a)    = Left (f a)
inLeft _ (Right a)   = Right a
