-- For Test only
{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Data.PTree.PTree where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isAsciiLower)
import qualified Data.Vector as V
import Prelude hiding (lookup, null)

-- For Test only
import Data.Char (ord)
import Test.QuickCheck

type Key = C.ByteString

type SuffixList a = V.Vector (PTree a)

data PTree a = Empty
             | Lf Key a
             | Br Key (Maybe a) (SuffixList a)
    deriving Show

empty :: PTree a
empty = Empty

null :: PTree a -> Bool
null Empty = True
null _     = False

member :: Key -> PTree a -> Bool
member k t = case lookup k t of
    Just _  -> True
    Nothing -> False

notMember :: Key -> PTree a -> Bool
notMember k t = not $ member k t

insert :: Key -> a -> PTree a -> PTree a
insert k v t
        | null t = Lf k v
        | k == p = setValue v t
        | otherwise = case trimPrefix p k of
            Just k' -> insertChild k' v t 
            Nothing -> join (Lf k v) t
    where p = getPrefix t

delete :: Key -> PTree a -> PTree a
delete k (Lf p v)
    | k == p = Empty
delete _ t = t

lookup :: Key -> PTree a -> Maybe a
lookup k t
        | null t = Nothing
        | p == k = getValue t
        | otherwise = case trimPrefix p k of
            Just k' -> lookup k' $ getChild k' t
            Nothing -> Nothing 
    where p = getPrefix t 

-- Helper Code

getPrefix :: PTree a -> Key
getPrefix (Lf p _) = p
getPrefix (Br p _ _) = p

trimPrefix :: Key -> Key -> Maybe Key
trimPrefix x y
    | x `C.isPrefixOf` y = Just $ trimPrefix' x y
    | otherwise = Nothing

-- Assumes prefix has already been checked
trimPrefix' :: Key -> Key -> Key
trimPrefix' x y = C.drop (C.length x) y

setKey :: Key -> PTree a -> PTree a
setKey k (Br _ x c) = Br k x c
setKey k (Lf _ x) = Lf k x

getKey :: PTree a -> Key
getKey (Br p _ _) = p
getKey (Lf p _)   = p

setValue :: a -> PTree a -> PTree a
setValue v (Br p _ c) = Br p (Just v) c
setValue v (Lf p _) = Lf p v

getValue :: PTree a -> Maybe a
getValue (Br _ x _) = x
getValue (Lf _ x)   = Just x

join x y = Br cp Nothing (V.replicate 26 empty V.// [(ik xp, x), (ik yp, y)])
    where
        (cp, xp, yp) = commonPrefix (getKey x) (getKey y)
        x' = setKey xp x
        y' = setKey yp y
        ik x = index x

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

insertChild :: Key -> a -> PTree a -> PTree a
insertChild k v (Br p x c) = Br p x (c V.// [(ik, insert k v $ c V.! ik)])
    where ik = index k
insertChild k v (Lf p x) = insertChild k v $ Br p (Just x) (V.replicate 26 empty)

getChild :: Key -> PTree a -> PTree a
getChild k (Br _ _ c) = c V.! (index k) 
getChild _ _          = Empty

index k = (ord $ C.head k) - (ord 'a')

-- Quick Check Code

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

prop_delete_one k (v :: V) = notMember k $ delete k $ insert k v empty

main = do
    quickCheck prop_member_empty
    quickCheck prop_insert_one
    quickCheck prop_change_one
    quickCheck prop_delete_one
