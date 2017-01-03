{-# LANGUAGE OverloadedStrings #-}

module ParseIni
    ( INISectName (..)
    , INIKey
    , INIVal (..)
    , INISection
    , INIFile
    , parseIniFile
    , toSectName
    , toKey
    , lookupSection
    , lookupValue
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative


-- **** TYPES ****
-- These are the types you should use for the results of your parse.
-- Think carefully about what's going on here!

-- |INI files are separated into sections and subsections of key-value pairs.
-- We represent section and subsection identifiers with the INISectName type.
-- Section names are case insensitive strings; subsection names are case sensitive.
data INISectName = ISect    { iSect    :: B.ByteString }
                 | ISubsect { iSect    :: B.ByteString
                            , iSubsect :: B.ByteString }
    deriving (Eq, Ord, Show)

-- |Within each (sub)section, an INI file contains a set of keys and values.
-- Keys are case insensitive strings.
type INIKey = B.ByteString

-- |After parsing key-value pairs, each value should be assigned a type.
-- We represent these types via the @INIVal@ sum type.
data INIVal = IBool Bool
            | IInt Integer
            | IString B.ByteString
    deriving (Eq, Ord, Show)

-- |An @INISection@ is a map from @INIKey@s to @INIVal@s.
type INISection = M.Map INIKey [INIVal]

-- |An @INIFile@ is a map from @INISectName@s to @INISection@s.
type INIFile = M.Map INISectName INISection


-- **** INTERFACE ****
-- You need to implement these so that we can test your code!
--
-- Why? Because you shouldn't need to expose exactly the way that
-- you handle, e.g., case insensitive string matching in order for
-- someone to use your INI file parser.

-- |Given a section name and possibly a subsection name, return an
-- appropriate @INISectName@. This function accounts for the case
-- insensitivity of the section name.
toSectName :: String -> Maybe String -> INISectName
toSectName sect Nothing = ISect (C8.pack (map C.toLower sect))
toSectName sect (Just subsect) = ISubsect (C8.pack (map C.toLower sect)) (C8.pack subsect)

-- |Given a key name, return an appropriate @INIKey@. This function
-- accounts for the case insensitivity of the key name.
toKey :: String -> INIKey
toKey = C8.pack

-- |Look up a section in an @INIFile@.
lookupSection :: INISectName -> INIFile -> Maybe INISection
lookupSection = M.lookup

-- |Look up a value in an @INISection@.
lookupSValue :: INIKey -> INISection -> Maybe [INIVal]
lookupSValue = M.lookup 

-- |Look up a value in an @INIFile@.
lookupValue :: INIKey -> INISectName -> INIFile -> Maybe [INIVal]
lookupValue k0 s0 f0 = lookupSection s0 f0 >>= \s1 -> lookupSValue k0 s1 


-- **** PARSER ****

-- |Parse an INI file into an @INIFile@.
--
-- An INI file comprises a sequence of sections.
--
-- A section starts with a header, which declares the name of the section or subsection.
-- The header is followed by a sequence of key-value declarations.
--
-- Whitespace between and inside sections is ignored, as are comment lines, which
-- begin with @#@ or @;@.
parseIniFile :: B.ByteString -> Either String INIFile
parseIniFile = parseOnly iniFile

-- Your implementation goes here.
--
-- parseIniFile should return @Left errmsg@ on error,
-- or @Right parsedResult@ on success.

intSuffix :: M.Map Char Integer
intSuffix = M.fromList [('k', 2^10), ('M', 2^20), ('G', 2^30), ('T', 2^40), ('P', 2^50), ('E', 2^60)]

subsectEscape :: M.Map Char Char
subsectEscape = M.fromList [('\\', '\\'), ('\"', '\"'), ('t', '\t'), ('b', '\b')]

stringEscape :: M.Map Char Char
stringEscape = M.union (M.fromList [('n', '\n')]) subsectEscape

escapeChar mp = do
    char '\\' 
    c <- satisfy $ inClass (M.keys mp)
    return $ M.findWithDefault c c mp

iniSectName = (toSectName <$> ("[" *> skipSpace *> sect) <*> (option Nothing (Just <$> (skipSpace *> subsect)) <* skipSpace <* "]"))
    where sect = map C.toLower <$> (many1 (digit <|> letter_ascii <|> char '.' <|> char '-'))
          subsect = "\"" *> many1 ((satisfy $ notInClass "\n\"\\") <|> escapeChar subsectEscape) <* "\""

iniKey = (toKey <$> ((:) <$> letter_ascii <*> (many (digit <|> letter_ascii <|> char '-'))))

iniValue = toINIVal <$> stringListVal
    where boolVal = readBool <$> (stringCI "true" <|> stringCI "false" <|> stringCI "on" <|> stringCI "off" <|> stringCI "yes" <|> stringCI "no")
          readBool s = elem (map C.toLower (C8.unpack s)) ["true", "on", "yes"]
          intVal = readInteger <$> ((,,) <$> option '+' (char '+' <|> char '-') <*> many1 digit <*> option ' ' (satisfy $ inClass "kMGTPE"))
          readInteger (si, ds, su) = let num = (read ds) * (M.findWithDefault 1 su intSuffix) 
                                     in if si == '+' then num else -num
          stringListVal = (:) <$> unquotedStringVal <*> (concat <$> (many ((\x y->[x,y]) <$> quotedStringVal <*> unquotedStringVal)))
          quotedStringVal = concat <$> ("\"" *> (sepBy (many (escapeChar stringEscape <|> (satisfy $ notInClass "\\\""))) "\\\n") <* "\"")
          unquotedStringVal = concat <$> (sepBy (many (escapeChar stringEscape <|> (satisfy $ notInClass "\\\"\n#;"))) "\\\n")
          toINIVal ss | len == 1 = case parseOnly ((IBool <$> (boolVal <* endOfInput)) <|> (IInt <$> (intVal <* endOfInput))) (C8.pack (strip (head ss))) of
                                        Left _ -> IString $ C8.pack $ strip (head ss)
                                        Right res -> res
                      | len `mod` 2 == 0 = IString $ C8.pack (concat (lstrip (head ss): tail ss))
                      | otherwise = IString $ C8.pack (lstrip (head ss) ++ concat (tail $ L.take (len - 1) ss) ++ rstrip (last ss))
            where len = length ss
                  strip = lstrip . rstrip
                  lstrip = dropWhile (flip elem [' '])
                  rstrip = reverse . lstrip . reverse 

iniSection = foldl addINIKeyAndVal M.empty 
            <$> many ((,) <$> (skipIgnore *> iniKey) <*> (option (IBool True) (skipSpace *> "=" *> iniValue)))
    where addINIKeyAndVal section (key, val) = M.insert key (val:(M.findWithDefault [] key section)) section

iniFile = foldl addINISection M.empty <$> (many ((,) <$> (skipIgnore *> iniSectName) <*> iniSection) <* skipIgnore <* endOfInput)
    where addINIKeyAndVals section (key, vals) = M.insert key (vals ++ (M.findWithDefault [] key section)) section
          addINISection iniFile (key, iniSection) = M.insert key (foldl addINIKeyAndVals (M.findWithDefault M.empty key iniFile) (M.toList iniSection)) iniFile

skipIgnore = skipMany (many1 space <|> commentLine <|> many1 (char '\n'))
    where commentLine = (char ';' <|> char '#') *> many (notChar '\n')