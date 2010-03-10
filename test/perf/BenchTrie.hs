{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import BenchCommon (Map(..), commonMain)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import qualified Data.Trie as T
import Prelude hiding (lookup)

type DataTrie = T.Trie Int

instance Map DataTrie where
    empty = T.empty
    null = T.null
    insert = T.insert
    delete = T.delete
    lookup k = fromJust . T.lookup k
    keys = T.keys
    prefixes k m = prefixes' k m []
        where
            prefixes' k m a
                | C.null k = a
                | T.member k m = prefixes' (C.take (C.length k - 1) k) (T.submap k m) (k:a)
                | otherwise    = prefixes' (C.take (C.length k - 1) k) m a

main :: IO ()
main = commonMain (empty :: DataTrie)
