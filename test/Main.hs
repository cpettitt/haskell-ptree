{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isAsciiLower, ord)
import Data.PTree.PTree as P
import qualified Data.Vector as V
import Prelude hiding (lookup, null)
import Test.QuickCheck
import Text.Printf

instance Arbitrary P.Key where
    arbitrary = C.pack `fmap` (listOf $ choose ('a', 'z'))

instance CoArbitrary P.Key where
    coarbitrary = coarbitrary . C.unpack

type V = Int

prop_member_empty k = P.notMember k P.empty

prop_insert_one k (v :: V) = (P.lookup k $ P.insert k v P.empty) == Just v

prop_change_one k (v1 :: V) (v2 :: V) =
    v1 /= v2 ==>
        (P.lookup k $ P.insert k v2 $ P.insert k v1 P.empty) == Just v2

tests = [("member_empty", quickCheck prop_member_empty)
        ,("insert_one", quickCheck prop_insert_one)
        ,("change_one", quickCheck prop_change_one)
        ]

main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests
