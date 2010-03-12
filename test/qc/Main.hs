{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.PTree as P

import Test.QuickCheck

import Text.Printf

import QuickCheckUtils

{--------------------------------------------------------------------
  Model Tests
--------------------------------------------------------------------}
prop_modelNull            = P.null            `eq1`     M.null
prop_modelSize            = P.size            `eq1`     M.size
prop_modelMember          = P.member          `eq2`     M.member
prop_modelNotMember       = P.notMember       `eq2`     M.notMember
prop_modelLookup          = P.lookup          `eq2`     M.lookup
prop_modelFindWithDefault = P.findWithDefault `eq3`     M.findWithDefault
prop_modelInsert          = P.insert          `listEq3` M.insert
prop_modelInsertWith      = P.insertWith      `listEq4` M.insertWith $ (+)
prop_modelInsertWith'     = P.insertWith'     `listEq4` M.insertWith' $ (+)
prop_modelInsertWithKey   = P.insertWithKey   `listEq4` M.insertWithKey $ (\k v v' -> C.length k + v + v')
prop_modelInsertWithKey'  = P.insertWithKey'  `listEq4` M.insertWithKey' $ (\k v v' -> C.length k + v + v')
prop_modelDelete          = P.delete          `listEq2` M.delete
prop_modelFold            = P.fold            `eq3`     M.fold $ (+)
prop_modelFoldWithKey     = P.foldWithKey     `eq3`     M.foldWithKey $ (\k v a -> C.length k + v + a)
prop_modelElems           = P.elems           `eq1`     M.elems
prop_modelKeys            = P.keys            `eq1`     M.keys
prop_modelKeysSet         = P.keysSet         `eq1`     M.keysSet
prop_modelAssocs          = P.assocs          `eq1`     M.assocs
prop_modelShow            = show              `eq1`     show -- tentative, if Map.show ever changed...

prop_modelFromList (l :: L) = P.toList (P.fromList l) == M.toList (M.fromList l)

prop_modelFromListWith (l :: L) = P.toList (P.fromListWith f l) == M.toList (M.fromListWith f l)
    where
        f v v' = v + v'

prop_modelFromListWithKey (l :: L) = P.toList (P.fromListWithKey f l) == M.toList (M.fromListWithKey f l)
    where
        f k v v' = C.length k + v + v'

{--------------------------------------------------------------------
  Idempotent Tests
--------------------------------------------------------------------}
prop_idemInsert (t :: T) k v = P.insert k v t == P.insert k v (P.insert k v t)
prop_idemDelete (t :: T) k   = P.delete k t == P.delete k (P.delete k t)

{--------------------------------------------------------------------
  Reversible Tests
--------------------------------------------------------------------}
prop_revInsert (t :: T) k v = P.notMember k t ==> P.delete k (P.insert k v t) == t
prop_revToList (t :: T)     = P.fromList (P.toList t) == t

{--------------------------------------------------------------------
  Other Tests
--------------------------------------------------------------------}
prop_null (t :: T) = if P.size t == 0 then P.null t else not $ P.null t

prop_singleton k (v :: Int) = P.singleton k v == P.insert k v P.empty

prop_member1 k = P.notMember k P.empty
prop_member2 (t :: T) k = not (P.notMember k t && P.member k t)

prop_insert (t :: T) k v = P.lookup k (P.insert k v t) == Just v

prop_delete (t :: T) = not (P.null t) ==> let k = head $ P.keys t in
    P.notMember k (P.delete k t)

prop_findWithDefault (t :: T) k def = P.notMember k t ==> P.findWithDefault def k t == def

prop_bang (t :: T) k v = P.insert k v t P.! k == v

prop_keys (t :: T) = P.keys t == keyList
    where
        keyList = map fst $ P.toList t

prop_size (t :: T) = length (P.toList t) == P.size t

prop_prefix1 = sort (P.prefixes (C.pack "roman") t) == map C.pack ["roma", "roman"]
    where
        t = fromStrList [("roman", 1), ("romans", 2), ("roma", 3)]
prop_prefix2 (t :: T) k = P.prefixes k t == filter (`C.isPrefixOf` k) (P.keys t)

prop_insertWith (t :: T) k v v2 = P.insertWith (+) k v2 (P.insert k v t) P.! k == v + v2

prop_insertWith' (t :: T) k v v2 = P.insertWith' (+) k v2 (P.insert k v t) P.! k == v + v2

{--------------------------------------------------------------------
  Main
--------------------------------------------------------------------}
main = do
    let check s a = printf "%-25s: " s >> quickCheck a
    let group s = putStrLn "" >> putStrLn s >> putStrLn (replicate (length s) '=')

    group "Model Tests"
    check "modelNull"            prop_modelNull
    check "modelSize"            prop_modelSize
    check "modelMember"          prop_modelMember
    check "modelNotMember"       prop_modelNotMember
    check "modelLookup"          prop_modelLookup
    check "modelFindWithDefault" prop_modelFindWithDefault
    check "modelInsert"          prop_modelInsert
    check "modelInsertWith"      prop_modelInsertWith
    check "modelInsertWith'"     prop_modelInsertWith'
    check "modelInsertWithKey"   prop_modelInsertWithKey
    check "modelInsertWithKey'"  prop_modelInsertWithKey'
    check "modelDelete"          prop_modelDelete
    check "modelFold"            prop_modelFold 
    check "modelFoldWithKey"     prop_modelFoldWithKey
    check "modelElems"           prop_modelElems
    check "modelKeys"            prop_modelKeys
    check "modelKeysSet"         prop_modelKeysSet
    check "modelAssocs"          prop_modelAssocs
    check "modelShow"            prop_modelShow
    check "modelFromList"        prop_modelFromList
    check "modelFromListWith"    prop_modelFromListWith
    check "modelFromListWithKey" prop_modelFromListWithKey

    group "Idempotent Tests"
    check "idemInsert"           prop_idemInsert
    check "idemDelete"           prop_idemDelete

    group "Reversible Tests"
    check "revInsert"            prop_revInsert
    check "revToList"            prop_revToList

    group "Other Tests"
    check "null"                 prop_null
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
    check "insertWith"           prop_insertWith
    check "insertWith'"          prop_insertWith'
