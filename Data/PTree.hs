module Data.PTree where

import Control.Arrow (first)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.Word
import Prelude hiding (foldr, lookup, null)
import qualified Prelude

type Key = S.ByteString

data PTree a = Tip
             | Node Key (Maybe a) (IM.IntMap (PTree a))

instance (Show a) => Show (PTree a) where
    show = show . map (first S.unpack) . toList

instance (Eq a) => Eq (PTree a) where
    (==) = (==) `on` toList

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
foldr = foldrNode S.empty

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
node k v = Node k v IM.empty

trimPrefix :: Key -> Key -> Maybe Key
trimPrefix x y
    | x `S.isPrefixOf` y = Just $ trimPrefix' x y
    | otherwise = Nothing

-- Assumes prefix has already been checked
trimPrefix' :: Key -> Key -> Key
trimPrefix' = SU.unsafeDrop . S.length

join :: PTree a -> PTree a -> PTree a
join (Node xk xv xc) (Node yk yv yc) = insertChild x' $ insertChild y' $ node ck Nothing
    where
        (ck, xk', yk') = commonPrefix xk yk
        x' = Node xk' xv xc
        y' = Node yk' yv yc
join _ _ = error "join: can't join Tip"

insertChild :: PTree a -> PTree a -> PTree a
insertChild x@(Node xk _ _) (Node yk yv yc) = Node yk yv (IM.insert (toChildKey xk) x yc)
insertChild _ _ = error "insertChild: Cannot insert child for Tip"

commonPrefix :: Key -> Key -> (Key, Key, Key)
commonPrefix x y = (c, x', y')
    where
        c = S.unfoldr f (x, y)
        f :: (Key, Key) -> Maybe (Word8, (Key, Key))
        f (xa, ya)
            | S.length xa == 0 || S.length ya == 0 = Nothing
            | xc == yc = Just (xc, (S.tail xa, S.tail ya))
            | otherwise = Nothing
            where
                xc = S.head xa
                yc = S.head ya
        x' = trimPrefix' c x
        y' = trimPrefix' c y

getChild :: Key -> PTree a -> PTree a
getChild k (Node _ _ xc) = IM.findWithDefault Tip (toChildKey k) xc
getChild _ _ = error "getChild: Cannot get child for Tip"

foldrNode :: Key -> (Key -> a -> b -> b) -> b -> PTree a -> b
foldrNode _ _ z Tip = z
foldrNode p f z (Node k v c) = Prelude.foldr step z' $ map snd $ IM.toList c
    where
        p' = p `S.append` k
        z' = case v of
            Just v' -> f p' v' z
            Nothing -> z
        step x a = foldrNode p' f a x

toChildKey :: Key -> IM.Key
toChildKey = fromIntegral . S.head
