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
import Data.List( sort )

import Control.Monad
import Control.Applicative
import Control.Parallel.Strategies
import Control.Arrow

import Prelude

--import qualified Data.IntMap as Map
import qualified Data.Map as Map


-- functions to benchmark

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
--                       ("sort",onSorted),
--                       ("rsort",onRevsorted)
                      ]
          sortFunctions = [("id",id),
                          ("sort",sort),
                          ("treeSort",treeSort),
                          ("yhcSort", yhcSort),
                          ("yhcSort2", yhcSort2),
                          ("countSort",countSort)]
    
     
      sortData <- sequence [ do { x <- f sampleSize ; nfIO (return x) ; return (t,x) } | (t,f) <- sortData' ]
    
      let benches = [ bgroup sortName [ bench fName (nf fFun sortBatch) | (fName,fFun) <- sortFunctions ]
                      | (sortName,sortBatch) <- sortData]
          bglobal = (bgroup "sorting" benches)
                    
          myoutput = S.fromList [ CSV, SVG 500 500, PDF 500 500, PNG 500 500 ]
          myplot = fromMap . M.fromList $ [ (KernelDensity,myoutput), (Timing,myoutput) ]
          conf = defaultConfig { cfgPlot = myplot, cfgSamples = ljust 500 }
      withConfig conf (do env <- measureEnvironment
                          runAndAnalyse (const True) env bglobal
                      )

randomSeq :: Int -> IO [Int]
randomSeq elNum = withSystemRandom (\gen -> replicateM elNum (uniform gen))
sortedSeq elNum = yhcSort <$> randomSeq elNum
reversSeq elNum = (reverse . yhcSort) <$> randomSeq elNum

