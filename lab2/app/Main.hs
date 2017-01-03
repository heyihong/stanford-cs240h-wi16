-- | Run ParseIni on input file, pretty printing the results
module Main where

import ParseIni
import PrettyPrintIni

import qualified Data.ByteString as B
import System.Environment
import System.Exit

-- |Main - parse input file, then pretty-print the result
main :: IO ()
main = do
    args <- getArgs
    input <- B.readFile (args !! 0)
    let result = parseIniFile input
    either (\err -> putStrLn err >> exitFailure)
           (\success -> (B.putStr $ prettyPrint success) >> exitSuccess)
           result
