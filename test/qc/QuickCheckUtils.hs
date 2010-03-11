{-# LANGUAGE TypeSynonymInstances #-}

module QuickCheckUtils where

import Control.Arrow (first)
import Control.Monad (liftM)

import qualified Data.ByteString.Char8 as C
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.PTree as P

import Test.QuickCheck

type T = P.PTree Int
type M = M.Map P.Key Int
type L = [(P.Key, Int)]

instance Arbitrary a => Arbitrary (P.PTree a) where
    arbitrary = liftM P.fromList arbitrary

instance CoArbitrary a => CoArbitrary (P.PTree a) where
    coarbitrary = coarbitrary . P.toList

instance Arbitrary P.Key where
    arbitrary = C.pack `fmap` listOf (choose ('a', 'z'))

instance CoArbitrary P.Key where
    coarbitrary = coarbitrary . C.unpack

eq1 :: (Eq a) => (T -> a)
              -> (M -> a)
              -> [(P.Key, Int)]
              -> Bool
eq1 f g l = f t == g m
    where
        t = P.fromList l
        m = M.fromList l

eq2 :: (Eq a) => (b -> T -> a)
              -> (b -> M -> a)
              -> b
              -> [(P.Key, Int)]
              -> Bool
eq2 f g x l = f x t == g x m
    where
        t = P.fromList l
        m = M.fromList l

eq3 :: (Eq a) => (b -> c -> T -> a)
              -> (b -> c -> M -> a)
              -> b
              -> c
              -> [(P.Key, Int)]
              -> Bool
eq3 f g x y l = f x y t == g x y m
    where
        t = P.fromList l
        m = M.fromList l

eq4 :: (Eq a) => (b -> c -> d -> T -> a)
              -> (b -> c -> d -> M -> a)
              -> b
              -> c
              -> d
              -> [(P.Key, Int)]
              -> Bool
eq4 f g x y z l = f x y z t == g x y z m
    where
        t = P.fromList l
        m = M.fromList l

listEq2 :: (b -> T -> T)
        -> (b -> M -> M)
        -> b
        -> [(P.Key, Int)]
        -> Bool
listEq2 f g = eq2 (\x' l' -> sortList . P.toList $ f x' l')
                  (\x' l' -> sortList . M.toList $ g x' l')

listEq3 :: (b -> c -> T -> T)
        -> (b -> c -> M -> M)
        -> b
        -> c
        -> [(P.Key, Int)]
        -> Bool
listEq3 f g = eq3 (\x' y' l' -> sortList . P.toList $ f x' y' l')
                  (\x' y' l' -> sortList . M.toList $ g x' y' l')

listEq4 :: (b -> c -> d -> T -> T)
        -> (b -> c -> d -> M -> M)
        -> b
        -> c
        -> d
        -> [(P.Key, Int)]
        -> Bool
listEq4 f g = eq4 (\x' y' z' l' -> sortList . P.toList $ f x' y' z' l')
                  (\x' y' z' l' -> sortList . M.toList $ g x' y' z' l')

sortList = sortBy (comparing fst)

fromStrList :: [(String, Int)] -> T
fromStrList = P.fromList . map (first C.pack)
