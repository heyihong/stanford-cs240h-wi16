module MergeIni (
    mergeINI
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Maybe as Maybe

import ParseIni

fileToSectAndKey :: INIFile -> Set (INISectName, INIKey)
fileToSectAndKey ini = Set.fromList $ foldr (++) [] $ map (\x -> combine (fst x) (sectToKey (snd x))) (Map.toList ini)
    where sectToKey sect = map fst (Map.toList sect) 
          combine sectName keys = map (\key -> (sectName, key)) keys

mergeVal :: Maybe [INIVal] -> Maybe [INIVal] -> Maybe [INIVal] -> Maybe [INIVal]
mergeVal base head merge | head == merge = head
                      | base == head = merge
                      | base == merge = head
                      | Maybe.isNothing head = merge
                      | Maybe.isNothing merge = head
                      | otherwise = Just $ lcpWithUnique (Maybe.fromJust head) (Maybe.fromJust merge)
                      where lcpWithUnique [] ms = ms
                            lcpWithUnique hs [] = hs
                            lcpWithUnique (h:hs) (m:ms) = if h == m then h:(lcpWithUnique hs ms) else [m, h]

updateINIFile :: INIFile -> (INISectName, INIKey, Maybe [INIVal]) -> INIFile
updateINIFile ini (sectName, key, (Just vals)) = Map.insert sectName newSect ini
    where newSect = Map.insert key vals (Map.findWithDefault Map.empty sectName ini)
updateINIFile ini (_, _, Nothing) = ini


-- | Merge two INI files given a common ancestor.
mergeINI :: INIFile             -- ^ Our version of the file (HEAD)
         -> INIFile             -- ^ The merge base (common ancestor)
         -> INIFile             -- ^ The version we are merging in
         -> INIFile
mergeINI headFile baseFile mergeFile = foldl updateINIFile Map.empty (map mergeSectAndKey sectAndKeyList)
    where sectAndKeyList = Set.toList $ Set.union (fileToSectAndKey headFile) (fileToSectAndKey mergeFile)
          mergeSectAndKey (sec, key) = (sec, key, mergeVal base head merge)
            where base = lookupValue key sec baseFile
                  head = lookupValue key sec headFile
                  merge = lookupValue key sec mergeFile 




