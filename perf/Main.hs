module Main (main) where

import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.PTree as P
import qualified Data.Trie as T

datamap :: Int -> IO ()
datamap n = do
    d <- C.readFile "testdata"
    let k = C.lines d
    let t = foldl' (\a x -> M.insert x 1 a) (M.empty :: M.Map C.ByteString Int) k
    putStr $ show $ foldl' (\a x -> (fromJust $ M.lookup x t) + a) 0 $ take (n * (length k)) $ cycle k

dataptree :: Int ->  IO ()
dataptree n = do
    d <- C.readFile "testdata"
    let k = C.lines d
    let t = foldl' (\a x -> P.insert x 1 a) (P.empty :: P.PTree Int) k
    putStr $ show $ foldl' (\a x -> (fromJust $ P.lookup x t) + a) 0 $ take (n * (length k)) $ cycle k

datatrie :: Int -> IO ()
datatrie n = do
    d <- C.readFile "testdata"
    let k = C.lines d
    let t = foldl' (\a x -> T.insert x 1 a) (T.empty :: T.Trie Int) k
    putStr $ show $ foldl' (\a x -> (fromJust $ T.lookup x t) + a) 0 $ take (n * (length k)) $ cycle k

main :: IO ()
main = defaultMain [
        bench "Data.PTree 1"  $ dataptree 1
      , bench "Data.Map 1"    $ datamap 1
      , bench "Data.Trie 1"   $ datatrie 1
      , bench "Data.PTree 10" $ dataptree 10
      , bench "Data.Map 10"   $ datamap 10
      , bench "Data.Trie 10"  $ datatrie 10
      ]
