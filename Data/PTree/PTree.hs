-- For Test only
{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Data.PTree.PTree where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isAsciiLower, ord)
import qualified Data.Vector as V
import Prelude hiding (lookup, null)

-- For Test only
--import Test.QuickCheck

type Key = C.ByteString

type SuffixList a = V.Vector (PTree a)

data PTree a = Tip
             | Node Key (Maybe a) (SuffixList a)
    deriving Show

empty :: PTree a
empty = Tip

null :: PTree a -> Bool
null Tip = True
null _   = False

member :: Key -> PTree a -> Bool
member k t = case lookup k t of
    Just _  -> True
    Nothing -> False

notMember :: Key -> PTree a -> Bool
notMember k = not . member k

insert :: Key -> a -> PTree a -> PTree a
insert k v t
        | null t = node k (Just v)
        | k == p = setValue v t
        | otherwise = case trimPrefix p k of
            Just k' -> insertChild (insert k' v $ getChild k' t) t
            Nothing -> join (node k (Just v)) t
    where p = getKey t

lookup :: Key -> PTree a -> Maybe a
lookup k t
        | null t = Nothing
        | p == k = getValue t
        | otherwise = case trimPrefix p k of
            Just k' -> lookup k' $ getChild k' t
            Nothing -> Nothing
    where p = getKey t

-- Helper Code

node :: Key -> Maybe a -> PTree a
node k v = Node k v (V.replicate 26 Tip)

trimPrefix :: Key -> Key -> Maybe Key
trimPrefix x y
    | x `C.isPrefixOf` y = Just $ trimPrefix' x y
    | otherwise = Nothing

-- Assumes prefix has already been checked
trimPrefix' :: Key -> Key -> Key
trimPrefix' = C.drop . C.length

setKey :: Key -> PTree a -> PTree a
setKey k (Node _ x c) = Node k x c

getKey :: PTree a -> Key
getKey (Node p _ _) = p

setValue :: a -> PTree a -> PTree a
setValue v (Node p _ c) = Node p (Just v) c

getValue :: PTree a -> Maybe a
getValue (Node _ x _) = x

join :: PTree a -> PTree a -> PTree a
join x y = insertChild x' $ insertChild y' $ node cp Nothing
    where
        (cp, xp, yp) = commonPrefix (getKey x) (getKey y)
        x' = setKey xp x
        y' = setKey yp y

insertChild :: PTree a -> PTree a -> PTree a
insertChild x (Node p v c) = Node p v (c V.// [(index $ getKey x, x)])

commonPrefix :: Key -> Key -> (Key, Key, Key)
commonPrefix x y = (c, x', y')
    where
        c = C.unfoldr f (x, y)
        f :: (Key, Key) -> Maybe (Char, (Key, Key))
        f (xa, ya)
            | C.length xa == 0 || C.length ya == 0 = Nothing
            | xc == yc = Just (xc, (C.tail xa, C.tail ya))
            | otherwise = Nothing
            where
                xc = C.head xa
                yc = C.head ya
        x' = trimPrefix' c x
        y' = trimPrefix' c y

getChild :: Key -> PTree a -> PTree a
getChild k (Node _ _ c) = c V.! index k

index k = ord (C.head k) - ord 'a'

-- Quick Check Code
{-
instance Arbitrary Key where
    arbitrary = C.pack `fmap` (listOf $ choose ('a', 'z'))

instance CoArbitrary Key where
    coarbitrary = coarbitrary . C.unpack

type V = Int

prop_member_empty k = notMember k empty

prop_insert_one k (v :: V) = (lookup k $ insert k v empty) == Just v

prop_change_one k (v1 :: V) (v2 :: V) =
    v1 /= v2 ==>
        (lookup k $ insert k v2 $ insert k v1 empty) == Just v2

main = do
    quickCheck prop_member_empty
    quickCheck prop_insert_one
    quickCheck prop_change_one
-}
