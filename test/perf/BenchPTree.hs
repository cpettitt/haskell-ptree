{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import BenchCommon (Map(..), commonMain)
import Data.Maybe (fromJust)
import qualified Data.PTree as P
import Prelude hiding (lookup)

type DataPTree = P.PTree Int

instance Map DataPTree where
    empty = P.empty
    null = P.null
    insert = P.insert
    delete = P.delete
    lookup k = fromJust . P.lookup k
    keys = P.keys
    prefixes = P.prefixes

main :: IO ()
main = commonMain (empty :: DataPTree)
