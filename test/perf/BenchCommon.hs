module BenchCommon (Map(..), commonMain) where

import Control.Parallel.Strategies (NFData(..))
import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Prelude hiding (lookup, null)

class Map a where
    empty :: a
    null :: a -> Bool
    insert :: C.ByteString -> Int -> a -> a
    delete :: C.ByteString -> a -> a
    lookup :: C.ByteString -> a -> Int
    keys :: a -> [C.ByteString]
    prefixes :: C.ByteString -> a -> [C.ByteString]

instance NFData C.ByteString where
    rnf x = x `seq` ()

-- | Performs a single lookup in the supplied map for every key in
--   [C.ByteString].
benchLookup :: (Map a) => [C.ByteString] -> a -> Int
benchLookup xs m = foldl' (\a x -> lookup x m + a) 0 xs

-- | Inserts every string from [C.ByteString] to the supplied Map.
benchInsert :: (Map a) => [C.ByteString] -> a -> Bool
benchInsert xs = null . insertStrings xs

-- | Benchmark the delete operation over all keys in the Map.
benchDelete :: (Map a) => [C.ByteString] -> a -> Bool
benchDelete xs m = null $ foldl' (flip delete) m xs

-- | Benchmarks the time it takes to retrieve all keys from the supplied
--   Map.
benchKeys :: (Map a) => a -> [C.ByteString]
benchKeys = keys

-- | Benchmarks the time it takes to look up all prefixes of a key.
benchPrefixes :: (Map a)  => a -> [C.ByteString]
benchPrefixes = prefixes $ C.pack "apprenticeship"

-- | Inserts every string from [C.ByteString] into the supplied Map.
insertStrings :: (Map a) => [C.ByteString] -> a -> a
insertStrings xs m = foldl' (\a x -> insert x 1 a) m xs

loadData :: String -> IO [C.ByteString]
loadData f = fmap C.lines $ C.readFile ("data/" ++ f ++ ".data")

testConfigs :: [String]
testConfigs = [ t ++ "-" ++ show n ++ "k" | t <- ["seq", "rnd"], n <- [1, 10] ]

commonMain :: (Map a) => a -> IO ()
commonMain e = do
    keys <- mapM loadData testConfigs
    let fullMaps = map (\x -> let !sx = insertStrings (map C.copy x) e in sx) keys
    let ckf = zip3 testConfigs keys fullMaps
    defaultMain
                [ bgroup "lookup"   $ map (\(c, xs, fm) -> bench c $ nf (benchLookup xs) fm) ckf
                , bgroup "insert"   $ map (\(c, xs, _)  -> bench c $ nf (benchInsert xs) e) ckf
                , bgroup "delete"   $ map (\(c, xs, fm) -> bench c $ nf (benchDelete xs) fm) ckf
                , bgroup "keys"     $ map (\(c, _, fm)  -> bench c $ nf benchKeys fm) ckf
                , bgroup "prefixes" $ map (\(c, _, fm)  -> bench c $ nf benchPrefixes fm) ckf
                ]
