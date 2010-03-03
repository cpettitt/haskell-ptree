module BenchCommon (Map(..), commonMain) where

import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Prelude hiding (lookup)

class Map a where
    empty :: a
    insert :: C.ByteString -> Int -> a -> a
    lookup :: C.ByteString -> a -> Int

-- | Inserts every string from [C.ByteString] to the supplied Map.
benchInsert :: (Map a) => [C.ByteString] -> a -> IO ()
benchInsert xs m = return (insertStrings xs m) >>= putStrLn . show . lookup (head xs)

-- | Performs a single lookup in the supplied map for every key in
--   [C.ByteString].
benchLookup :: (Map a) => [C.ByteString] -> a -> IO ()
benchLookup xs m = putStrLn $ show $ foldl' (\a x -> lookup x m + a) 0 xs

-- | Inserts every string from [C.ByteString] to the supplied Map and returns
--   the Map through the IO Monad.
insertStrings :: (Map a) => [C.ByteString] -> a -> a
insertStrings xs m = foldl' (\a x -> insert x 1 a) m xs

loadData :: String -> IO ([C.ByteString])
loadData f = C.readFile ("data/" ++ f ++ ".data") >>= return . C.lines

testConfigs :: [String]
testConfigs = [ (t ++ "-" ++ (show n) ++ "k") | t <- ["seq", "rnd"], n <- [1, 10] ]

commonMain :: (Map a) => a -> IO ()
commonMain e = do
    keys <- mapM loadData testConfigs
    let fullMaps = map (\x -> let !sx = insertStrings (map C.copy x) e in sx) keys
    let configAndKeys = zip3 testConfigs keys fullMaps
    defaultMain
                [ bgroup "insert" $ map (\(c, xs, _) -> bench c $ benchInsert xs e) configAndKeys
                , bgroup "lookup" $ map (\(c, xs, fm) -> bench c $ benchLookup xs fm) configAndKeys
                ]
