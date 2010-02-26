module Data.PTree.PTree where

import Control.Arrow (first)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (ord)
import qualified Data.Vector as V
import Prelude hiding (foldr, lookup, null)
import qualified Prelude as Prelude

type Key = C.ByteString

data PTree a = Tip
             | Node Key (Maybe a) (V.Vector (PTree a))

instance (Show a) => Show (PTree a) where
    show = show . map (first C.unpack) . toList

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

keys :: PTree a -> [Key]
keys = foldr step []
    where step k _ = (k:)

toList :: PTree a -> [(Key, a)]
toList = foldr (\k v -> ((k,v):)) []

foldr :: (Key -> a -> b -> b) -> b -> PTree a -> b
foldr = foldrNode C.empty

insert :: Key -> a -> PTree a -> PTree a
insert k v Tip = node k (Just v)
insert k v n@(Node nk _ nc)
        | k == nk = Node nk (Just v) nc
        | otherwise = case trimPrefix nk k of
            Just k' -> insertChild (insert k' v $ getChild k' n) n
            Nothing -> join (node k (Just v)) n

lookup :: Key -> PTree a -> Maybe a
lookup _ Tip = Nothing
lookup k n@(Node nk nv _)
        | k == nk = nv
        | otherwise = case trimPrefix nk k of
            Just k' -> lookup k' $ getChild k' n
            Nothing -> Nothing

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

join :: PTree a -> PTree a -> PTree a
join (Node xk xv xc) (Node yk yv yc) = insertChild x' $ insertChild y' $ node ck Nothing
    where
        (ck, xk', yk') = commonPrefix xk yk
        x' = Node xk' xv xc
        y' = Node yk' yv yc
join _ _ = error "join: can't join Tip"

insertChild :: PTree a -> PTree a -> PTree a
insertChild x@(Node xk _ _) (Node yk yv yc) = Node yk yv (yc V.// [(index xk, x)])
insertChild _ _ = error "insertChild: Cannot insert child for Tip"

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
getChild k (Node _ _ xc) = xc V.! index k
getChild _ Tip = error "getChild: Cannot get child for Tip"

index :: Key -> Int
index k = ord (C.head k) - ord 'a'

foldrNode :: Key -> (Key -> a -> b -> b) -> b -> PTree a -> b
foldrNode _ _ z Tip = z
foldrNode p f z (Node k v c) = Prelude.foldr step z' $ V.toList c
    where
        p' = p `C.append` k
        z' = case v of
            Just v' -> f p' v' z
            Nothing -> z
        step x a = foldrNode p' f a x
