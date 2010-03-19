-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PTree
-- Copyright   :  Copyright (c) 2010 Chris Pettitt
-- License     :  MIT
-- Maintainer  :  cpettitt@gmail.com
--
-- An efficient Patricia tree implementation that maps strict 'ByteString's
-- to values.
--
-- The interface for this module is intended to look similar to that for
-- 'Data.Map' and 'Data.IntMap', where appropriate. It differs from
-- 'Data.Map' in the restriction of the key type to 'ByteString's, its
-- functions for looking up all prefixes of a key, and improved looked up
-- performance. It differs from 'Data.IntMap' in that it supports variable
-- length keys.
--
-- This module is intended to be imported @qualified@ to avoid clashes with
-- functions exported in 'Prelude'. For example:
--
-- > import qualified Data.PTree as P
--
-- Function comments include the Big-O time complexity. In many cases a
-- function has a worst-time complexity of /O(min(n, S))/. This means that
-- the function's time complexity is linear with respect to the minimum
-- of either /n/, the number of elements in the tree, or /S/, the length of
-- the 'ByteString' supplied as a key to the function.
-----------------------------------------------------------------------------

module Data.PTree (
        -- * Types
          PTree, Key

        -- * Constructors
        , empty
        , singleton

        -- * Operators
        , (!)

        -- * Queries
        , null
        , size
        , member
        , notMember
        , lookup
        , findWithDefault

        -- * Insertion
        , insert
        , insertWith, insertWith'
        , insertWithKey, insertWithKey'

        -- * Deletion
        , delete

        -- * Update
        , adjust
        , adjustWithKey
        , update
        , updateWithKey

        -- * Traversal
        -- ** Map
        , map
        , mapWithKey

        -- ** Fold
        , fold
        , foldWithKey

        -- * Conversions
        , elems
        , keys
        , keysSet
        , assocs
        , prefixes

        -- * Lists
        , toList
        , fromList
        , fromListWith
        , fromListWithKey
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as SU
import Data.Char (chr)
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Word
import Prelude hiding (foldr, lookup, map, null)


{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- | A map of ByteStrings to values.
data PTree a = Nil
             | Node {-# UNPACK #-} !Key (Maybe a) !(Children a)

-- | The key type for PTrees.
type Key = S.ByteString

type ChildKey = IM.Key
type Children a = IM.IntMap (PTree a)

instance (Show a) => Show (PTree a) where
    show = showString "fromList " . show . toList

instance (Eq a) => Eq (PTree a) where
    (==) = (==) `on` toList

{--------------------------------------------------------------------
  Constructors
--------------------------------------------------------------------}

-- | /O(1)/ Constructs an empty tree.
empty :: PTree a
empty = Nil

-- | /O(1)/ Constructs a tree with a single element.
singleton :: Key -> a -> PTree a
singleton k v = node k (Just v)

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | /O(min(n, S))/ Find the value for the given key. Calls 'error'
--   when the key is not in the tree
(!) :: PTree a -> Key -> a
t ! k = fromMaybe
            (error $ "PTree.!: key " ++ show k ++ " is not an element of this PTree")
            (lookup k t)

{--------------------------------------------------------------------
  Queries
--------------------------------------------------------------------}

-- | /O(1)/ Tests whether the tree is empty.
null :: PTree a -> Bool
null Nil = True
null _   = False

-- | /O(n)/ Returns the number of elements in the tree.
size :: PTree a -> Int
size = foldWithKey (\_ _ a -> a + 1) 0

-- | /O(min(n, S))/ Determines if the supplied key is an element in
--   the supplied tree.
member :: Key -> PTree a -> Bool
member k t = case lookup k t of
    Just _  -> True
    Nothing -> False

-- | /O(min(n, S))/ Determines if the supplied key is NOT an element
--   in the supplied tree.
notMember :: Key -> PTree a -> Bool
notMember k = not . member k

-- | /O(min(n, S))/ Searches for the given key in the tree.
lookup :: Key -> PTree a -> Maybe a
lookup _ Nil = Nothing
lookup k (Node nk nv nc)
        | k == nk = nv
        | lk <= lnk = Nothing
        | otherwise = lookup k $ getChild (getChildKey k lnk) nc
    where
        lk = S.length k
        lnk = S.length nk

-- | /O(min(n, S))/ Searches for the value for the given key. If the
--   key is not in the tree then the default value is returned.
findWithDefault :: a -> Key -> PTree a -> a
findWithDefault def k t = fromMaybe def (lookup k t)

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(min(n, S))/ Inserts the given key/value pair into the tree.
insert :: Key -> a -> PTree a -> PTree a
insert k v Nil = node k (Just v)
insert k v n@(Node nk _ nc)
        | k == nk = Node nk (Just v) nc
        | nk `S.isPrefixOf` k = updateChild (insert k v) n k
        | otherwise = join (node k (Just v)) n

-- | /O(min(n, S))/ Inserts with the given function which combines
--   the new value with an old value. @'insertWith' f key value tree@
--   will insert @value@ into the @tree@ if it does not contain @key@.
--   If it does contain @key@, the function will insert the value @f
--   value old_value@.
insertWith :: (a -> a -> a) -> Key -> a -> PTree a -> PTree a
insertWith f = insertWithKey (\_ -> f)

-- | /O(min(n, S))/ Strict version of 'insertWith'
insertWith' :: (a -> a -> a) -> Key -> a -> PTree a -> PTree a
insertWith' f = insertWithKey' (\_ -> f)

-- | /O(min(n, S))/ Inserts with the given function which combines
--   the key, new value, and old value. @'insertWithKey' f key value
--   tree@ will insert @value@ into the @tree@ if it does not contain
--   @key@. If it does contain @key@, the function will insert the
--   value @f key value old_value@.
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> PTree a -> PTree a
insertWithKey _ k v Nil = node k (Just v)
insertWithKey f k v n@(Node nk nv nc)
        | k == nk = case nv of
            Nothing -> Node nk (Just v) nc
            Just x  -> Node nk (Just $ f k v x) nc
        | nk `S.isPrefixOf` k = updateChild (insertWithKey f k v) n k
        | otherwise = join (node k (Just v)) n

-- | /O(min(n, S))/ Strict version of 'insertWithKey'
insertWithKey' :: (Key -> a -> a -> a) -> Key -> a -> PTree a -> PTree a
insertWithKey' _ k v Nil = node k (Just v)
insertWithKey' f k v n@(Node nk nv nc)
        | k == nk = case nv of
            Nothing -> Node nk (Just v) nc
            Just x  -> let x' = f k v x in x' `seq` Node nk (Just x') nc
        | nk `S.isPrefixOf` k = updateChild (insertWithKey' f k v) n k
        | otherwise = join (node k (Just v)) n

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

-- | /O(min(n, S))/ Removes the given key from the tree.
delete :: Key -> PTree a -> PTree a
delete _ Nil = Nil
delete k n@(Node nk _ _)
        | k == nk = collapse n
        | nk `S.isPrefixOf` k = updateChild (delete k) n k
        | otherwise = n

{--------------------------------------------------------------------
  Update
--------------------------------------------------------------------}

adjust :: (a -> a) -> Key -> PTree a -> PTree a
adjust f = adjustWithKey (\_ v -> f v)

adjustWithKey :: (Key -> a -> a) -> Key -> PTree a -> PTree a
adjustWithKey f = updateWithKey (\k v -> Just (f k v))

update :: (a -> Maybe a) -> Key -> PTree a -> PTree a
update f = updateWithKey (\_ v -> f v)

updateWithKey :: (Key -> a -> Maybe a) -> Key -> PTree a -> PTree a
updateWithKey _ _ Nil = Nil
updateWithKey f k n@(Node nk nv nc)
    | k == nk = case nv of
            Nothing -> n
            Just nv' -> case f k nv' of
                Nothing -> collapse n
                Just v' -> Node nk (Just v') nc
    | nk `S.isPrefixOf` k = updateChild (updateWithKey f k) n k
    | otherwise = n

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | /O(n)/ Map a function over all values in the tree.
map :: (a -> b) -> PTree a -> PTree b
map f = mapWithKey (\_ v -> f v)

-- | /O(n)/ Map a function over all keys and values in the tree.
mapWithKey :: (Key -> a -> b) -> PTree a -> PTree b
mapWithKey _ Nil = Nil
mapWithKey f (Node k v c) = Node k v' c'
    where
        v' = case v of
            Nothing -> Nothing
            Just x  -> Just (f k x)
        c' = IM.map (mapWithKey f) c

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

-- | /O(n)/ Folds the values in the tree.
fold :: (a -> b -> b) -> b -> PTree a -> b
fold f = foldWithKey (\_ -> f)

-- | /O(n)/ Folds the keys and values in the tree.
foldWithKey :: (Key -> a -> b -> b) -> b -> PTree a -> b
foldWithKey _ z Nil = z
foldWithKey f z (Node k v c) = case v of
            Nothing -> children
            Just x  -> f k x children
    where
        children = IM.fold step z c
        step x a = foldWithKey f a x

{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}

-- | /O(n)/ Return all values of the tree in ascending order of their
--   keys.
elems :: PTree a -> [a]
elems = fold (:) []

-- | /O(n)/ Return all keys in the tree in ascending order.
keys :: PTree a -> [Key]
keys = foldWithKey (\k _ a -> k:a) []

-- | /O(n)/ Return the set of all keys in the tree.
keysSet :: PTree a -> Set.Set Key
keysSet = Set.fromDistinctAscList . keys

-- | /O(n) Return all key/value pairs in the tree in ascending order.
assocs :: PTree a -> [(Key, a)]
assocs = toList

-- | /O(min(n, S))/ Returns all keys in this tree that are a prefix
--   of the given Key. The keys are returned in ascending order.
--
-- > prefixes "roman" (fromList [("roman", 1), ("romans", 2), ("roma", 3)]) == ["roma", "roman"]
prefixes :: Key -> PTree a -> [Key]
prefixes k t = reverse $ go k t []
    where
        go :: Key -> PTree a -> [Key] -> [Key]
        go _ Nil a = a
        go k' (Node nk nv nc) a
            | S.length k < lnk = a
            | nk `S.isPrefixOf` k' = case nv of
                Nothing -> next a
                Just _  -> next (nk:a)
            | otherwise = a
                where
                    lnk = S.length nk
                    next = go k' (getChild (getChildKey k' lnk) nc)

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n)/ Converts the tree to a list of key/value pairs in
--  ascending order.
toList :: PTree a -> [(Key, a)]
toList = foldWithKey (\k v -> ((k,v):)) []

-- | /O(n * min(n, S))/ Create a tree from a list of key/value pairs.
fromList :: [(Key, a)] -> PTree a
fromList = foldl' ins empty
    where
        ins t (k, v) = insert k v t

-- | /O(n * min(n, S))/ Create a tree from a list of key/value pairs
--   with the given combining function.
fromListWith :: (a -> a -> a) -> [(Key, a)] -> PTree a
fromListWith f = fromListWithKey (\_ v v' -> f v v')

-- | /O(n * min(n, S))/ Create a tree from a list of key/value pairs
--   with the given combining function.
fromListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> PTree a
fromListWithKey f = foldl' ins empty
    where
        ins t (k, v) = insertWithKey f k v t

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

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
join _ _ = error "PTree.join: can't join Nil"

{-# INLINE updateChild #-}
updateChild :: (PTree a -> PTree a) -> PTree a -> Key -> PTree a
updateChild f (Node pk pv pc) k = Node pk pv newChildren
    where
        childKey = getChildKey k (S.length pk)
        child = getChild childKey pc
        newChildren = case f child of
            Nil -> IM.delete childKey pc
            n   -> IM.insert childKey n pc
updateChild _ Nil _ = error "PTree.updateChild: can't update child of Nil"

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
getChild = IM.findWithDefault Nil

-- Set the value of this node to Nothing and attempts to collapse it.
-- If a node has 0 children then it can be collapsed to 'Nil'. If a
-- node has 1 child, then we can remove this node and promote the
-- child to this node's position.
collapse :: PTree a -> PTree a
collapse Nil = Nil
collapse (Node k _ c)
    | IM.null c = Nil
    | IM.size c == 1 = snd $ head $ IM.toList c
    | otherwise = Node k Nothing c

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}

showTree :: (Show a) => PTree a -> String
showTree t = showsTree [bar] t ""

showsTree :: (Show a) => [String] -> PTree a -> ShowS
showsTree bars Nil = showString "Nil\n"
showsTree bars (Node nk nv nc) = showNode nk nv . IM.foldWithKey showChild (showString "") nc
    where
        showNode _ Nothing  = showString " *\n"
        showNode k (Just v) = showString " * " . showString (C.unpack k) . showString " := " . showString (show v) . showString "\n"
        showChild k n r = showBars bars . showString "[" . showString (chr k:"") . showString "] -- " . showsTree (" |     ":bars) n . r

showBars :: [String] -> ShowS
showBars []   = id
showBars bars = showString (concat (reverse (tail bars)))

bar :: String
bar = " |     "
