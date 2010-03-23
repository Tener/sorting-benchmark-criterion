{-# OPTIONS_GHC -O2 -XBangPatterns -XTypeSynonymInstances -XTypeOperators #-}
module Main where

import Criterion
import Criterion.MultiMap
import qualified Data.Map as M
import qualified Data.Set as S
import Criterion.Monad
import Criterion.Config
import Criterion.Plot
import Criterion.Environment


import System.IO
import System.Environment
import System.Random
import System.Random.MWC
import System.Exit
import Data.List( sort )

import Control.Monad
import Control.Applicative
import Control.Parallel.Strategies
import Control.Arrow

import Data.Array.Vector ( fromU )

import QsortUvector
import SortFFI

import Prelude

--import qualified Data.IntMap as Map
import qualified Data.Map as Map


-- functions to benchmark

-- qsort is stable and does not concatenate.
qsort xs = qsort_iv' (compare) xs []

qsort_iv' _   []     r = r
qsort_iv' _   [x]    r = x:r
qsort_iv' cmp (x:xs) r = qpart cmp x xs [] [] r

-- qpart partitions and sorts the sublists
qpart cmp x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort_iv' cmp rlt (x:rqsort_iv' cmp rge r)
qpart cmp x (y:ys) rlt rge r =
    case cmp x y of
	GT -> qpart cmp x ys (y:rlt) rge r
        _  -> qpart cmp x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort_iv' _   []     r = r
rqsort_iv' _   [x]    r = x:r
rqsort_iv' cmp (x:xs) r = rqpart cmp x xs [] [] r

rqpart cmp x [] rle rgt r =
    qsort_iv' cmp rle (x:qsort_iv' cmp rgt r)
rqpart cmp x (y:ys) rle rgt r =
    case cmp y x of
	GT -> rqpart cmp x ys rle (y:rgt) r
    	_  -> rqpart cmp x ys (y:rle) rgt r



treeSort = concatMap (reverse . snd) . Map.toAscList
           . Map.fromListWith (++) . map (\x -> (x,[x]))

countSort = concatMap (\xn -> case xn of (x,n) -> replicate n x)
           . Map.toAscList . Map.fromListWith (+) . map (\x -> (x,1))


yhcSort :: (Ord a) => [a] -> [a]
yhcSort = sortByYhc compare

sortByYhc cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = {-# SCC "desc/2" #-} descending b [a]  xs
      | otherwise       = {-# SCC "asc/2" #-} ascending  b [a] xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = {-# SCC "desc/3" #-} descending b (a:as) bs
    descending a as bs  = {-# SCC "desc/4" #-} (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = {-# SCC "asc/3" #-} ascending b (a:as) bs
    ascending a as bs   = {-# SCC "asc/4" #-} rev as [a] : sequences bs

    rev (x:xs) ys = rev xs (x:ys)
    rev [] ys = ys

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = {-# SCC "mergePairs/2" #-} merge a b: mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as


--

yhcSort2 :: (Ord a) => [a] -> [a]
yhcSort2 = sortByYhc2 compare

sortByYhc2 cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = {-# SCC "desc/2" #-} descending b [a]  xs
      | otherwise       = {-# SCC "asc/2" #-} ascending  b [a] xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = {-# SCC "desc/3" #-} descending b (a:as) bs
    descending a as bs  = {-# SCC "desc/4" #-} (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = {-# SCC "asc/3" #-} ascending b (a:as) bs
    ascending a as bs   = {-# SCC "asc/4" #-} rev as [a] : sequences bs

    rev (x:xs) ys = rev xs (x:ys)
    rev [] ys = ys

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = {-# SCC "mergePairs/2" #-} merge a b: mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as


-----------------
-- criterion code

main = 
    do

      let sampleSize = 100000
          sortData' = [("rand",randomSeq)
--                       ,("sort",sortedSeq)
--                       ,("rsort",rsortedSeq)
                      ]
          sortFunctions = [("id",id),
                           ("qsort",qsort),
--                           ("qsort_uv1",qsort_uv1),
--                           ("qsort_uv2",qsort_uv2),
--                           ("qsort_uv3",qsort_uv3),
                           ("qsort_uv4",qsort_uv4),
                           ("qsort_uv5",qsort_uv5),
                           ("qsort_ffi",qsort_ffi),
                           ("sort",sort),
                           ("treeSort",treeSort),
                           ("yhcSort", yhcSort),
                           ("yhcSort2", yhcSort2),
                           ("countSort",countSort)]
    
      putStrLn "generating data to sort"
      sortData <- sequence [ do { x <- f sampleSize ; nfIO (return x) ; return (t,x) } | (t,f) <- sortData' ]
      -- exitFailure
    
      let benches = [ bgroup sortName [ bench fName (nf fFun sortBatch) | (fName,fFun) <- sortFunctions ]
                      | (sortName,sortBatch) <- sortData]
          bglobal = (bgroup "sorting" benches)
                    
          myoutput = S.fromList [ CSV, SVG 500 500, PDF 500 500, PNG 500 500 ]
          myplot = fromMap . M.fromList $ [ (KernelDensity,myoutput), (Timing,myoutput) ]
          conf = defaultConfig { cfgPlot = myplot }
      withConfig conf (do env <- measureEnvironment
                          runAndAnalyse (const True) env bglobal
                      )

force mf = do
  x <- mf
  return (x `seq` x)

randomSeq :: Int -> IO [Int]
randomSeq elNum = withSystemRandom (\gen -> fromU <$> uniformArray gen elNum)
sortedSeq elNum = yhcSort <$> randomSeq elNum
rsortedSeq elNum = (reverse . yhcSort) <$> randomSeq elNum

