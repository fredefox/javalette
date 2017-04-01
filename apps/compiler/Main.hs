module Main ( main ) where

import System.IO

main :: IO ()
main = putStrLnStderr "OK"

putStrStderr :: String -> IO ()
putStrStderr = hPutStr stderr

putStrLnStderr :: String -> IO ()
putStrLnStderr = hPutStrLn stderr
