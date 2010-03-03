{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Main where

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import Data.List ((\\))
import qualified Data.List as L
import Data.PTree
import Prelude hiding (lookup, null)
import Test.QuickCheck
import Text.Printf

instance Arbitrary a => Arbitrary (PTree a) where
    arbitrary = liftM fromList arbitrary

instance CoArbitrary a => CoArbitrary (PTree a) where
    coarbitrary = coarbitrary . toList

instance Arbitrary Key where
    arbitrary = C.pack `fmap` listOf (choose ('a', 'z'))

instance CoArbitrary Key where
    coarbitrary = coarbitrary . C.unpack

type T = PTree Int

prop_member_empty k = notMember k empty

prop_not_member (t :: T) k = not (notMember k t && member k t)

prop_insert (t :: T) k v = lookup k (insert k v t) == Just v

prop_insert_idem (t :: T) k v = insert k v t == insert k v (insert k v t)

prop_from_to_list (t :: T) = fromList (toList t) == t

prop_keys (t :: T) = L.null (keys t \\ keyList) && L.null (keyList \\ keys t)
    where
        keyList = map fst $ toList t

main = do
    let check s a = printf "%-25s: " s >> quickCheck a

    check "member_empty"    prop_member_empty
    check "not_member"      prop_not_member
    check "insert"          prop_insert
    check "insert_idem"     prop_insert_idem
    check "from_to_list"    prop_from_to_list
    check "keys"            prop_keys
