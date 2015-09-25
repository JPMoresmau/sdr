{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.SDR

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck

import qualified Data.Set as S
import qualified Data.IntSet as I

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [   testGroup "Properties" [
           testProperty "size" prop_size
         , testProperty "contains1" prop_contains1
         , testProperty "best1" prop_best1
         , testProperty "match1" prop_match1
         , testProperty "contains1 in Union All" prop_contains1Union
         , testProperty "imperfect1" prop_imperfect1
        ]
    ]

prop_size :: SDRTestData -> Bool
prop_size SDRTestData{..} = length tdKeys == size tdSdr

prop_contains1 :: SDRTestData -> Bool
prop_contains1 SDRTestData{..} = all (\x->contains tdSdr (encode tdSdr x) x) tdKeys

prop_best1 :: SDRTestData -> Bool
prop_best1 SDRTestData{..} = all (\x -> x== best tdSdr (encode tdSdr x)) tdKeys

prop_match1 :: SDRTestData -> Bool
prop_match1 SDRTestData{..} = all (\x -> tdMarkers == match tdSdr (encode tdSdr x) x) tdKeys

prop_contains1Union :: SDRTestData -> Bool
prop_contains1Union SDRTestData{..} =
    let  u= union tdSdr tdKeys
    in all (contains tdSdr u) tdKeys

prop_imperfect1 :: SDRTestData -> Bool
prop_imperfect1 SDRTestData{..} = all (\x -> x == best tdSdr (imperfect $ encode tdSdr x)) tdKeys
    where imperfect = I.fromList . tail . I.toList

data SDRTestData = SDRTestData
    { tdKeys :: [Int]
    , tdMarkers :: Int
    , tdSdr :: SDRSet Int

    } deriving (Show,Read,Eq,Ord)

instance Arbitrary SDRTestData where
    arbitrary = do
        sz <- choose (5,40)
        let keys = take sz [1..]
        markers <- choose (2,5)
        let total = markers * sz
        return $  SDRTestData keys markers (build total markers $ S.fromList keys)

