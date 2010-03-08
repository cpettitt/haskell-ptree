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

        -- * Lists
        , toList
        , fromList
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.List (foldl')
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
    show = showString "fromList " . show . toList

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

-- | Create a PTree from a list of key/value pairs.
fromList :: [(Key, a)] -> PTree a
fromList = foldl' ins empty
    where
        ins m (k, v) = insert k v m

-- | /O(n)/ Right-folds the values in the PTree.
foldr :: (Key -> a -> b -> b) -> b -> PTree a -> b
foldr _ z Tip = z
foldr f z (Node k v c) = Prelude.foldr step z' $ map snd $ IM.toList c
    where
        z' = case v of
            Just x -> f k x z
            Nothing -> z
        step x a = foldr f a x

-- | Inserts the given key/value pair into the PTree.
insert :: Key -> a -> PTree a -> PTree a
insert k v Tip = node k (Just v)
insert k v n@(Node nk _ nc)
        | k == nk = Node nk (Just v) nc
        | nk `S.isPrefixOf` k = insertChild (insert k v $ getChild (SU.unsafeIndex k (S.length nk)) nc) n
        | otherwise = join (node k (Just v)) n

-- | Searches for the given key in the PTree.
lookup :: Key -> PTree a -> Maybe a
lookup _ Tip = Nothing
lookup k (Node nk nv nc)
        | k == nk = nv
        | lk <= lnk = Nothing
        | otherwise = lookup k $ getChild (SU.unsafeIndex k lnk) nc
    where
        lk = S.length k
        lnk = S.length nk

-- Helper Code

node :: Key -> Maybe a -> PTree a
node k v = Node k v IM.empty

join :: PTree a -> PTree a -> PTree a
join x@(Node xk _ _) y@(Node yk _ _)
        | xk == ck = insertChild y x
        | yk == ck = insertChild x y
        | otherwise = insertChild x $ insertChild y $ node ck Nothing
    where
        ck = commonPrefix xk yk
join _ _ = error "join: can't join Tip"

insertChild :: PTree a -> PTree a -> PTree a
insertChild x@(Node xk xv _) (Node yk yv yc) = Node yk yv (IM.insert (fromIntegral $ SU.unsafeIndex xk (S.length yk)) x yc)
insertChild _ _ = error "insertChild: Cannot insert child for Tip"

commonPrefix :: Key -> Key -> Key
commonPrefix x y = S.unfoldr f (x, y)
    where
        f :: (Key, Key) -> (Maybe (Word8, (Key, Key)))
        f (xa, ya)
            | S.null xa || S.null ya = Nothing
            | xc == yc = Just (xc, (SU.unsafeTail xa, SU.unsafeTail ya))
            | otherwise = Nothing
            where
                xc = SU.unsafeHead xa
                yc = SU.unsafeHead ya

{-# INLINE getChild #-}
getChild :: Word8 -> Children a -> PTree a
getChild = IM.findWithDefault Tip . fromIntegral
