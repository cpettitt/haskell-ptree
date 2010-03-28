module Data.PTree.Internal (
      PTree(..)
    , Key
    , ChildKey
    , Children
    ) where

import qualified Data.ByteString as S
import qualified Data.IntMap as IM

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

