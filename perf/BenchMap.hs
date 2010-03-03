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
    insert = M.insert
    lookup k = fromJust . M.lookup k

main :: IO ()
main = commonMain (empty :: DataMap)