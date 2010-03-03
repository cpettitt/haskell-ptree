{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import BenchCommon (Map(..), commonMain)
import Data.Maybe (fromJust)
import qualified Data.Trie as T
import Prelude hiding (lookup)

type DataTrie = T.Trie Int

instance Map DataTrie where
    empty = T.empty
    insert = T.insert
    lookup k = fromJust . T.lookup k
    keys = T.keys

main :: IO ()
main = commonMain (empty :: DataTrie)
