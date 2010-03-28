module Data.PTree.Debug (
      showTree
    ) where

import Data.Char (chr)
import qualified Data.IntMap as IM
import Data.PTree
import Data.PTree.Internal
import qualified Data.ByteString.Char8 as C

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
