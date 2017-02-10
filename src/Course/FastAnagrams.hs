{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import           Course.Core
import           Course.Functor
import           Course.List
import qualified Data.Set       as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams string filename =
  let setA = S.fromList $ hlist $ permutations string
      ioAnagrams = S.intersection setA . S.fromList . hlist . lines <$> readFile filename
  in listh . S.toList <$> ioAnagrams

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
