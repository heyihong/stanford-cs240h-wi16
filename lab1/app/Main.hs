-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import Data.Maybe
import Control.Monad
import System.Environment
import System.Exit

import Tr

parseArgs :: [String] -> Maybe (CharSet, Maybe CharSet)
parseArgs ["-d", set1] 
    | not (null set1) = Just (set1, Nothing)
parseArgs [set1, set2]
    | not (null set1 || null set2) = Just (set1, Just set2)
parseArgs _ = Nothing

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    when (isNothing args) $ do
        putStrLn "Usage: tr [-d] charSet1 [charSet2]"
        exitWith $ ExitFailure 1
    interact $ uncurry tr $ fromJust args
