-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PTree
-- Copyright   :  Copyright (c) 2010 Chris Pettitt
-- License     :  MIT
-- Maintainer  :  cpettitt@gmail.com
--
-- A Patricia tree implementation that maps arbitrary length ByteStrings to
-- values.
--
-----------------------------------------------------------------------------

module Data.PTree (
        -- * Types
          PTree, Key

        -- * Construction
        , empty
        , singleton

        -- * Queries
        , (!)
        , null
        , size
        , member
        , notMember
        , lookup
        , findWithDefault
        , prefixes

        -- * Insertion
        , insert

        -- * Deletion
        , delete

        -- * Folds
        , foldWithKey

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
import Data.Maybe (fromMaybe)
import Data.Word
import Prelude hiding (foldr, lookup, null)
import qualified Prelude

-- | The key type for PTrees.
type Key = S.ByteString

type ChildKey = IM.Key
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

-- | /O(1)/ Constructs a PTree with a single element.
singleton :: Key -> a -> PTree a
singleton k v = node k (Just v)

-- | Find the value for the given key.
--   Calls 'error' when the key is not in the PTree
(!) :: PTree a -> Key -> a
t ! k = fromMaybe
            (error $ "PTree.!: key " ++ show k ++ " is not an element of this PTree")
            (lookup k t)

-- | /O(1)/ Tests whether the PTree is empty.
null :: PTree a -> Bool
null Tip = True
null _   = False

-- | /O(n)/ Returns the number of elements in the PTree.
size :: PTree a -> Int
size = foldWithKey (\_ _ a -> a + 1) 0

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
keys = foldWithKey step []
    where step k _ = (k:)

-- | /O(n)/ Converts the PTree to a list of key/value pairs.
toList :: PTree a -> [(Key, a)]
toList = foldWithKey (\k v -> ((k,v):)) []

-- | Create a PTree from a list of key/value pairs.
fromList :: [(Key, a)] -> PTree a
fromList = foldl' ins empty
    where
        ins t (k, v) = insert k v t

-- | /O(n)/ Folds the values in the PTree.
foldWithKey :: (Key -> a -> b -> b) -> b -> PTree a -> b
foldWithKey _ z Tip = z
foldWithKey f z (Node k v c) = IM.fold step z' c
    where
        z' = case v of
            Just x -> f k x z
            Nothing -> z
        step x a = foldWithKey f a x

-- | Inserts the given key/value pair into the PTree.
insert :: Key -> a -> PTree a -> PTree a
insert k v Tip = node k (Just v)
insert k v n@(Node nk _ nc)
        | k == nk = Node nk (Just v) nc
        | nk `S.isPrefixOf` k = updateChild (insert k v) n k
        | otherwise = join (node k (Just v)) n

-- | Removes the given key from the PTree
delete :: Key -> PTree a -> PTree a
delete _ Tip = Tip
delete k n@(Node nk _ nc)
        | k == nk = if IM.null nc
                        then Tip
                        else if IM.size nc == 1
                            then snd $ head $ IM.toList nc
                            else Node nk Nothing nc
        | nk `S.isPrefixOf` k = updateChild (delete k) n k
        | otherwise = n

-- | Searches for the given key in the PTree.
lookup :: Key -> PTree a -> Maybe a
lookup _ Tip = Nothing
lookup k (Node nk nv nc)
        | k == nk = nv
        | lk <= lnk = Nothing
        | otherwise = lookup k $ getChild (getChildKey k lnk) nc
    where
        lk = S.length k
        lnk = S.length nk

-- | Searches for the value for the given key. If the key is not in the PTree
--   then the default value is returned.
findWithDefault :: a -> Key -> PTree a -> a
findWithDefault def k t = fromMaybe def (lookup k t)

-- | Returns all keys in this PTree that are a prefix of the given Key.
--
-- > sort (prefixes "roman" (fromList [("roman", 1), ("romans", 2), ("roma", 3)])) == ["roma", "roman"]
prefixes :: Key -> PTree a -> [Key]
prefixes k t = go k t []
    where
        go :: Key -> PTree a -> [Key] -> [Key]
        go k Tip a = a
        go k (Node nk nv nc) a
            | (S.length k) < lnk = a
            | nk `S.isPrefixOf` k = case nv of
                Nothing -> next a
                Just _  -> next (nk:a)
            | otherwise = a 
                where
                    lnk = S.length nk
                    next = go k (getChild (getChildKey k lnk) nc)

-- Helper Code

node :: Key -> Maybe a -> PTree a
node k v = Node k v IM.empty

join :: PTree a -> PTree a -> PTree a
join x@(Node xk _ _) y@(Node yk _ _)
        | xk == ck = insertChild y yk x
        | yk == ck = insertChild x xk y
        | otherwise = insertChild x xk $ insertChild y yk $ node ck Nothing
    where
        ck = commonPrefix xk yk
        insertChild :: PTree a -> Key -> PTree a -> PTree a
        insertChild child key parent = updateChild (const child) parent key
join _ _ = error "PTree.join: can't join Tip"

{-# INLINE updateChild #-}
updateChild :: (PTree a -> PTree a) -> PTree a -> Key -> PTree a
updateChild f (Node pk pv pc) k = Node pk pv newChildren
    where
        childKey = getChildKey k (S.length pk)
        child = getChild childKey pc
        newChildren = case f child of
            Tip -> IM.delete childKey pc
            n   -> IM.insert childKey n pc
updateChild _ Tip _ = error "PTree.updateChild: can't update child of Tip"

commonPrefix :: Key -> Key -> Key
commonPrefix x y = S.unfoldr f (x, y)
    where
        f :: (Key, Key) -> Maybe (Word8, (Key, Key))
        f (xa, ya)
            | S.null xa || S.null ya = Nothing
            | xc == yc = Just (xc, (SU.unsafeTail xa, SU.unsafeTail ya))
            | otherwise = Nothing
            where
                xc = SU.unsafeHead xa
                yc = SU.unsafeHead ya

{-# INLINE getChildKey #-}
getChildKey :: Key -> Int -> ChildKey
getChildKey k = fromIntegral . SU.unsafeIndex k

{-# INLINE getChild #-}
getChild :: ChildKey -> Children a -> PTree a
getChild = IM.findWithDefault Tip
