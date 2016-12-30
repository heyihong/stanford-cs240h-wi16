-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import Tr
import System.Environment
import System.IO

repl :: (String -> String) -> IO ()
repl tr = do
    end <- isEOF
    if end 
        then putStr ""
        else do {line <- getLine; putStrLn $ tr line; repl tr}

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
    args <- getArgs	
    case length args of
        0 -> repl (tr "" Nothing)
        1 -> repl (tr (args !! 0) Nothing)
        _ -> repl (tr (args !! 0) (Just $ args !! 1))
