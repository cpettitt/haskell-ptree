{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.PTree as P

import Test.QuickCheck

import Text.Printf

import QuickCheckUtils

prop_modelNull            = P.null            `eq1`     M.null
prop_modelMember          = P.member          `eq2`     M.member
prop_modelNotMember       = P.notMember       `eq2`     M.notMember
prop_modelInsert          = P.insert          `listEq3` M.insert
prop_modelDelete          = P.delete          `listEq2` M.delete
prop_modelFindWithDefault = P.findWithDefault `eq3`     M.findWithDefault
prop_modelSize            = P.size            `eq1`     M.size

prop_idemInsert (t :: T) k v = P.insert k v t == P.insert k v (P.insert k v t)
prop_idemDelete (t :: T) k   = P.delete k t == P.delete k (P.delete k t)

prop_revInsert (t :: T) k v = P.notMember k t ==> P.delete k (P.insert k v t) == t
prop_revToList (t :: T)     = P.fromList (P.toList t) == t

prop_null1          = P.null P.empty
prop_null2 (t :: T) = P.size t > 0 ==> not $ P.null t

prop_singleton k (v :: Int) = P.singleton k v == P.insert k v P.empty

prop_member1 k = P.notMember k P.empty
prop_member2 (t :: T) k = not (P.notMember k t && P.member k t)

prop_insert (t :: T) k v = P.lookup k (P.insert k v t) == Just v

prop_delete (t :: T) = not (P.null t) ==> let k = head $ P.keys t in
    P.notMember k (P.delete k t)

prop_findWithDefault (t :: T) k def = P.notMember k t ==> P.findWithDefault def k t == def

prop_bang (t :: T) k v = (P.insert k v t) P.! k == v

prop_keys (t :: T) = sort (P.keys t) == sort (keyList)
    where
        keyList = map fst $ P.toList t

prop_size (t :: T) = length (P.toList t) == P.size t

prop_prefix1 = sort (P.prefixes (C.pack "roman") t) == map C.pack ["roma", "roman"]
    where
        t = fromStrList [("roman", 1), ("romans", 2), ("roma", 3)]
prop_prefix2 (t :: T) k = sort (P.prefixes k t) == sort (filter (flip C.isPrefixOf k) (P.keys t))

main = do
    let check s a = printf "%-25s: " s >> quickCheck a
    let group s = putStrLn "" >> putStrLn s >> putStrLn (replicate (length s) '=')

    group "Model Tests"
    check "modelNull"            prop_modelNull
    check "modelMember"          prop_modelMember
    check "modelNotMember"       prop_modelNotMember
    check "modelInsert"          prop_modelInsert
    check "modelDelete"          prop_modelDelete
    check "modelFindWithDefault" prop_modelFindWithDefault
    check "modelSize"            prop_modelSize

    group "Idempotent Tests"
    check "idemInsert"           prop_idemInsert
    check "idemDelete"           prop_idemDelete

    group "Reversible Tests"
    check "revInsert"            prop_revInsert
    check "revToList"            prop_revToList

    group "Other Tests"
    check "null1"                prop_null1
    check "null2"                prop_null2
    check "singleton"            prop_singleton
    check "member1"              prop_member1
    check "member2"              prop_member2
    check "insert"               prop_insert
    check "delete"               prop_delete
    check "findWithDefault"      prop_findWithDefault
    check "bang"                 prop_bang
    check "keys"                 prop_keys
    check "size"                 prop_size
    check "prefix1"              prop_prefix1
    check "prefix2"              prop_prefix2
