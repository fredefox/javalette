module Main ( main ) where

import System.IO
import System.Environment

import Javalette.Syntax.AbsJavalette
import Javalette.Parser ( parse )

main :: IO ()
main = do
  inp <- parseInput
  case parseProgram inp of
    Left{}  -> putStrLnErr "BAD"
    Right{} -> putStrLnErr "OK"

-- TODO: We might like to do something more fancy (e.g. using
-- optparse-applicative)
-- | The test-runner supplies a single argument that is the file to compile - so
-- we'll just read that.
parseInput :: IO String
parseInput = head <$> getArgs >>= readFile

parseProgram :: String -> Either String Prog
parseProgram = parse

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr
