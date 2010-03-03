-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PTree
-- Copyright   :  Copyright (c) 2010 Chris Pettitt
-- License     :  MIT
-- Maintainer  :  cpettitt@gmail.com
--
-- An efficient implementation of maps from @ByteString@s to values.
--
-----------------------------------------------------------------------------

module Data.PTree (
        -- * Types
          PTree, Key

        -- * Construction
        , empty

        -- * Queries
        , null
        , member
        , notMember
        , lookup

        -- * Insertion
        , insert

        -- * Folds
        , foldr

        -- * Conversions
        , keys
        , toList
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.Word
import Prelude hiding (foldr, lookup, null)
import qualified Prelude

-- | The key type for PTrees.
type Key = S.ByteString

type Children a = IM.IntMap (PTree a)

-- | A map of ByteStrings to values.
data PTree a = Tip
             | Node {-# UNPACK #-} !Key (Maybe a) !(Children a)

instance (Show a) => Show (PTree a) where
    show = show . toList

instance (Eq a) => Eq (PTree a) where
    (==) = (==) `on` toList

-- | /O(1)/ Constructs an empty PTree.
empty :: PTree a
empty = Tip

-- | /O(1)/ Tests whether the PTree is empty.
null :: PTree a -> Bool
null Tip = True
null _   = False

-- | Determines if the supplied key is an element in the supplied PTree.
member :: Key -> PTree a -> Bool
member k t = case lookup k t of
    Just _  -> True
    Nothing -> False

-- | Determines if the supplied key is NOT an element in the supplied PTree.
notMember :: Key -> PTree a -> Bool
notMember k = not . member k

-- | /O(n)/ Return all elements in the PTree.
keys :: PTree a -> [Key]
keys = foldr step []
    where step k _ = (k:)

-- | /O(n)/ Converts the PTree to a list of key/value pairs.
toList :: PTree a -> [(Key, a)]
toList = foldr (\k v -> ((k,v):)) []

-- | /O(n)/ Right-folds the values in the PTree.
foldr :: (Key -> a -> b -> b) -> b -> PTree a -> b
foldr = foldrNode S.empty
    where
        foldrNode :: Key -> (Key -> a -> b -> b) -> b -> PTree a -> b
        foldrNode _ _ z Tip = z
        foldrNode p f z (Node k v c) = Prelude.foldr step z' $ map snd $ IM.toList c
            where
                p' = p `S.append` k
                z' = case v of
                    Just v' -> f p' v' z
                    Nothing -> z
                step x a = foldrNode p' f a x

-- | Inserts the given key/value pair into the PTree.
insert :: Key -> a -> PTree a -> PTree a
insert k v Tip = node k (Just v)
insert k v n@(Node nk _ nc)
        | k == nk = Node nk (Just v) nc
        | otherwise = case trimPrefix nk k of
            Just k' -> insertChild (insert k' v $ getChild k' nc) n
            Nothing -> join (node k (Just v)) n

-- | Searches for the given key in the PTree.
lookup :: Key -> PTree a -> Maybe a
lookup _ Tip = Nothing
lookup k (Node nk nv nc)
        | k == nk = nv
        | otherwise = case trimPrefix nk k of
            Just k' -> lookup k' $ getChild k' nc
            Nothing -> Nothing

-- Helper Code

node :: Key -> Maybe a -> PTree a
node k v = Node k v IM.empty

trimPrefix :: Key -> Key -> Maybe Key
trimPrefix x y
    | x `S.isPrefixOf` y = Just $ unsafeTrimPrefix x y
    | otherwise = Nothing

-- Assumes prefix has already been checked
unsafeTrimPrefix :: Key -> Key -> Key
unsafeTrimPrefix = SU.unsafeDrop . S.length

join :: PTree a -> PTree a -> PTree a
join (Node xk xv xc) (Node yk yv yc) = insertChild x' $ insertChild y' $ node ck Nothing
    where
        (ck, xk', yk') = commonPrefix xk yk
        x' = Node xk' xv xc
        y' = Node yk' yv yc
join _ _ = error "join: can't join Tip"

insertChild :: PTree a -> PTree a -> PTree a
insertChild x@(Node xk xv _) (Node yk yv yc)
    | S.null xk = Node yk xv yc
    | otherwise = Node yk yv (IM.insert (toChildKey xk) x yc)
insertChild _ _ = error "insertChild: Cannot insert child for Tip"

commonPrefix :: Key -> Key -> (Key, Key, Key)
commonPrefix x y = (c, x', y')
    where
        c = S.unfoldr f (x, y)
        f :: (Key, Key) -> Maybe (Word8, (Key, Key))
        f (xa, ya)
            | S.null xa || S.null ya = Nothing
            | xc == yc = Just (xc, (SU.unsafeTail xa, SU.unsafeTail ya))
            | otherwise = Nothing
            where
                xc = SU.unsafeHead xa
                yc = SU.unsafeHead ya
        x' = unsafeTrimPrefix c x
        y' = unsafeTrimPrefix c y

{-# INLINE getChild #-}
getChild :: Key -> Children a -> PTree a
getChild = IM.findWithDefault Tip . toChildKey

toChildKey :: Key -> IM.Key
toChildKey = fromIntegral . SU.unsafeHead
