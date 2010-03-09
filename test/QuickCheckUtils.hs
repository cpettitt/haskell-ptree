{-# LANGUAGE TypeSynonymInstances #-}

module QuickCheckUtils where

import Control.Monad (liftM)

import qualified Data.ByteString.Char8 as C
import qualified Data.PTree as P

import Test.QuickCheck

type T = P.PTree Int
type L = [(P.Key, Int)]

instance Arbitrary a => Arbitrary (P.PTree a) where
    arbitrary = liftM P.fromList arbitrary

instance CoArbitrary a => CoArbitrary (P.PTree a) where
    coarbitrary = coarbitrary . P.toList

instance Arbitrary P.Key where
    arbitrary = C.pack `fmap` listOf (choose ('a', 'z'))

instance CoArbitrary P.Key where
    coarbitrary = coarbitrary . C.unpack
