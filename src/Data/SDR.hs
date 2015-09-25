-----------------------------------------------------------------------------
--
-- Module      :  Data.SDR
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

module Data.SDR (
    SDRSet(..)
  , Total
  , Markers
  , build
  , encode
  , union
  , contains
  , match
  , best
  , Data.SDR.null
  , Data.SDR.size
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntSet as I

import qualified Data.List as L
import Data.Maybe
import Data.Ord

type Total = Int

type Markers = Int

data SDRSet a = SDRSet
    { sdrTotal :: Total
    , sdrMarkers :: Markers
    , sdrMap :: M.Map a (I.IntSet)
    } deriving (Show,Read,Eq,Ord)

build :: Ord a => Total -> Markers -> S.Set a -> SDRSet a
build tot mrk keys
    | S.size keys > maxKeys tot mrk = error "Too many keys for SDR size"
    | otherwise = SDRSet tot mrk $ fst $ foldr bld (M.empty,0) keys
    where
        bld a (m,ix) = (M.insert a (newSet ix) m,ix + step)
        step = 1
        sz = S.size keys
        gap = tot `div` mrk
        next i =
            let n = i + gap
            in if n>=tot then n - tot else n
        ls = take mrk . iterate next
        newSet = I.fromList . ls

maxKeys :: Total -> Markers -> Int
maxKeys tot mrk = tot `div` mrk

union :: Ord a => SDRSet a -> [a] -> I.IntSet
union sdr = I.unions . catMaybes . map (`M.lookup` sdrMap sdr)

contains :: Ord a => SDRSet a -> I.IntSet -> a -> Bool
contains sdr vals key = match sdr vals key == sdrMarkers sdr

match :: Ord a => SDRSet a -> I.IntSet -> a -> Int
match sdr vals key = I.size $ I.intersection (encode sdr key) vals

null :: SDRSet a -> Bool
null = M.null . sdrMap

size ::SDRSet a -> Int
size = M.size . sdrMap

encode :: Ord a => SDRSet a -> a -> I.IntSet
encode sdr key = fromMaybe (error "unknown key in SDRSet") $ M.lookup key (sdrMap sdr)

best :: Ord a => SDRSet a -> I.IntSet -> a
best sdr vals
    | Data.SDR.null sdr = error "empty SDR passed to best"
    | otherwise =
        let as = M.assocs (sdrMap sdr)
            scores = map (\(a,i)-> (a,I.size $ I.intersection i vals)) as
        in fst $ L.maximumBy (comparing snd) scores

