{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintIni
    ( prettyPrint
    ) where

import ParseIni

-- Data.ByteString.Char8 and Data.ByteString both export the same (strict) ByteString type
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.List as L

-- |Pretty-print an @INIFile@ in INI format.
prettyPrint :: INIFile -> BC.ByteString
prettyPrint = M.foldWithKey (\k v s -> BC.concat [printINISectName k, printINISection v, s]) ""

escapeCharMap :: M.Map Char String
escapeCharMap = M.fromList [('\\', "\\\\"), ('\"', "\\\""), ('\t', "\\t"), ('\b', "\\b"), ('\n', "\\n")]

printString :: String -> BC.ByteString
printString s = BC.pack $ "\"" ++ (concat $ map (\c -> M.findWithDefault [c] c escapeCharMap) s) ++ "\""

printINISectName :: INISectName -> BC.ByteString
printINISectName (ISect {iSect = sect}) = BC.concat ["[", sect, "]\n"]
printINISectName (ISubsect {iSect = sect, iSubsect = subsect}) = BC.concat ["[", sect, " ", printString $ BC.unpack subsect, "]\n"]

printINISection :: INISection -> BC.ByteString
printINISection = M.foldWithKey (\k vs s -> BC.concat (map (printINIKeyAndVal k) (L.sort vs)) `BC.append` s) ""

printINIKeyAndVal :: INIKey -> INIVal -> BC.ByteString
printINIKeyAndVal key val = BC.concat ["    ", key, " = ", printINIVal val, "\n"]

printINIVal :: INIVal -> BC.ByteString
printINIVal (IBool b) = BC.pack $ show b
printINIVal (IInt i) = BC.pack $ show i
printINIVal (IString s) = printString $ BC.unpack s

