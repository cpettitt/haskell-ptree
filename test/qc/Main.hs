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
prop_modelInsertWith      = P.insertWith      `listEq4` M.insertWith     $ (+)
prop_modelInsertWith'     = P.insertWith'     `listEq4` M.insertWith'    $ (+)
prop_modelInsertWithKey   = P.insertWithKey   `listEq4` M.insertWithKey  $ sumKVV
prop_modelInsertWithKey'  = P.insertWithKey'  `listEq4` M.insertWithKey' $ sumKVV
prop_modelDelete          = P.delete          `listEq2` M.delete
prop_modelAdjust          = P.adjust          `listEq3` M.adjust         $ (+1)
prop_modelAdjustWithKey   = P.adjustWithKey   `listEq3` M.adjustWithKey  $ sumKV
prop_modelUpdate          = P.update          `listEq3` M.update         $ (\v -> Just $ v+1)
prop_modelUpdateWithKey   = P.updateWithKey   `listEq3` M.updateWithKey  $ (\k v -> Just $ C.length k + v + 1)
prop_modelMap             = P.map             `listEq2` M.map            $ (+1)
prop_modelMapWithKey      = P.mapWithKey      `listEq2` M.mapWithKey     $ sumKV
prop_modelFold            = P.fold            `eq3`     M.fold           $ (+)
prop_modelFoldWithKey     = P.foldWithKey     `eq3`     M.foldWithKey    $ sumKVV
prop_modelElems           = P.elems           `eq1`     M.elems
prop_modelKeys            = P.keys            `eq1`     M.keys
prop_modelKeysSet         = P.keysSet         `eq1`     M.keysSet
prop_modelAssocs          = P.assocs          `eq1`     M.assocs
prop_modelShow            = show              `eq1`     show -- tentative, if Map.show ever changed...

prop_modelUnion (l1 :: L) (l2 :: L) =
    P.toList (P.union (P.fromList l1) (P.fromList l2)) == M.toList (M.union (M.fromList l1) (M.fromList l2))

prop_modelUnionWithKey (l1 :: L) (l2 :: L) =
        P.toList (P.unionWithKey sumKVV t1 t2) == M.toList (M.unionWithKey sumKVV m1 m2)
    where
        t1 = P.fromList l1
        t2 = P.fromList l2
        m1 = M.fromList l1
        m2 = M.fromList l2

prop_modelUnions (ls :: [L]) =
    P.toList (P.unions $ map P.fromList ls) == M.toList (M.unions $ map M.fromList ls)

prop_modelUnionsWith (ls :: [L]) =
    P.toList (P.unionsWith sumVV $ map P.fromList ls) == M.toList (M.unionsWith sumVV $ map M.fromList ls)

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
prop_singleton k (v :: Int) = P.singleton k v == P.insert k v P.empty

prop_bang (t :: T) k v = P.insert k v t P.! k == v

prop_null (t :: T) = if P.size t == 0 then P.null t else not $ P.null t

prop_size (t :: T) = length (P.toList t) == P.size t

prop_member1 k = P.notMember k P.empty
prop_member2 (t :: T) k = not (P.notMember k t && P.member k t)
prop_member3 (t :: T) k v = P.member k (P.insert k v t)

prop_findWithDefault (t :: T) k def = P.notMember k t ==> P.findWithDefault def k t == def

prop_insert (t :: T) k v = P.lookup k (P.insert k v t) == Just v
prop_insertWith (t :: T) k v v2 = P.insertWith (+) k v2 (P.insert k v t) P.! k == v + v2
prop_insertWith' (t :: T) k v v2 = P.insertWith' (+) k v2 (P.insert k v t) P.! k == v + v2
prop_insertWithKey (t :: T) k v v2 = P.insertWithKey sumKVV k v2 (P.insert k v t) P.! k == sumKVV k v v2
prop_insertWithKey' (t :: T) k v v2 = P.insertWithKey' sumKVV k v2 (P.insert k v t) P.! k == sumKVV k v v2

prop_delete (t :: T) = not (P.null t) ==> let k = head $ P.keys t in
    P.notMember k (P.delete k t)

prop_updateWithKey1 (t :: T) k v = P.notMember k $ P.updateWithKey (\_ _ -> Nothing) k (P.insert k v t)
prop_updateWithKey2 (t :: T) k v = P.updateWithKey (\k -> Just . sumKV k) k (P.insert k v t) P.! k == sumKV k v

prop_unionWithKey (t1 :: T) (t2 :: T) k v1 v2 =
    P.unionWithKey sumKVV (P.insert k v1 t1) (P.insert k v2 t2) P.! k == sumKVV k v1 v2

prop_keys (t :: T) = P.keys t == keyList
    where
        keyList = map fst $ P.toList t

prop_prefixes (t :: T) k = P.prefixes k t == filter (`C.isPrefixOf` k) (P.keys t)

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
    check "modelAdjust"          prop_modelAdjust
    check "modelAdjustWithKey"   prop_modelAdjustWithKey
    check "modelUpdate"          prop_modelUpdate
    check "modelUpdateWithKey"   prop_modelUpdateWithKey
    check "modelUnion"           prop_modelUnion
    check "modelUnionWithKey"    prop_modelUnionWithKey
    check "modelUnions"          prop_modelUnions
    check "modelUnionsWith"      prop_modelUnionsWith
    check "modelMap"             prop_modelMap
    check "modelMapWithKey"      prop_modelMapWithKey
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
    check "singleton"            prop_singleton
    check "bang"                 prop_bang
    check "null"                 prop_null
    check "size"                 prop_size
    check "member1"              prop_member1
    check "member2"              prop_member2
    check "member3"              prop_member3
    check "findWithDefault"      prop_findWithDefault
    check "insert"               prop_insert
    check "insertWith"           prop_insertWith
    check "insertWith'"          prop_insertWith'
    check "insertWithKey"        prop_insertWithKey
    check "insertWithKey'"       prop_insertWithKey'
    check "delete"               prop_delete
    check "updateWithKey1"       prop_updateWithKey1
    check "updateWithKey2"       prop_updateWithKey2
    check "unionWithKey"         prop_unionWithKey
    check "keys"                 prop_keys
    check "prefixes"             prop_prefixes
