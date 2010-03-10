{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import BenchCommon (Map(..), commonMain)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Prelude hiding (lookup)

type DataMap   = M.Map C.ByteString Int

instance Map DataMap where
    empty = M.empty
    null = M.null
    insert = M.insert
    delete = M.delete
    lookup k = fromJust . M.lookup k
    keys = M.keys
    prefixes k m = prefixes' k m []
        where
            prefixes' k m a
                | C.null k = a
                | M.member k m = prefixes' (C.take (C.length k - 1) k) m (k:a)
                | otherwise    = prefixes' (C.take (C.length k - 1) k) m a

main :: IO ()
main = commonMain (empty :: DataMap)
