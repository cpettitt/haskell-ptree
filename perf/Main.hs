module Main (main) where

import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.PTree as P
import qualified Data.Trie as T
import System (getArgs)

datamap :: [C.ByteString] -> IO ()
datamap d = do
            let t = foldl' (\a x -> M.insert x 1 a) (M.empty :: M.Map C.ByteString Int) d
            putStrLn $ show $ foldl' (\a x -> (fromJust $ M.lookup x t) + a) 0 $ take (100 * (length d)) $ cycle d

ptree :: [C.ByteString] -> IO ()
ptree d = do
            let t = foldl' (\a x -> P.insert x 1 a) (P.empty :: P.PTree Int) d
            putStrLn $ show $ foldl' (\a x -> (fromJust $ P.lookup x t) + a) 0 $ take (100 * (length d)) $ cycle d

trie :: [C.ByteString] -> IO ()
trie d = do
            let t = foldl' (\a x -> T.insert x 1 a) (T.empty :: T.Trie Int) d
            putStrLn $ show $ foldl' (\a x -> (fromJust $ T.lookup x t) + a) 0 $ take (100 * (length d)) $ cycle d

cmd :: String -> [C.ByteString] -> IO ()
cmd "datamap" d = datamap d
cmd "ptree"   d = ptree d
cmd "trie"    d = trie d
cmd c         _ = error $ "Unrecognized command: " ++ c

main :: IO ()
main = do
    args <- getArgs
    d <- C.getContents >>= return . C.lines
    case length args of
        0 -> error "Please specify the test type (one of datamap, ptree, trie)"
        _ -> cmd (head args) d
