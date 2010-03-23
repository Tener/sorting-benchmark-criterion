module QsortUvector ( qsort_uv1, qsort_uv2, qsort_uv3, qsort_uv4, qsort_uv5 ) where

import Data.Array.Vector
import Data.Array.Vector.Algorithms.Combinators ( apply )
import qualified Data.Array.Vector.Algorithms.Intro as Intro
import qualified Data.Array.Vector.Algorithms.Merge as Merge
import qualified Data.Array.Vector.Algorithms.Radix as Radix
import qualified Data.Array.Vector.Algorithms.TriHeap as TriHeap

qsort_uv1 :: [Int] -> [Int]
qsort_uv1 lst = fromU (qsort1 (toU lst))

qsort_uv2 :: [Int] -> [Int]
qsort_uv2 lst = fromU (qsort2 (toU lst))

qsort_uv3 :: [Int] -> [Int]
qsort_uv3 lst = fromU (qsort3 (toU lst))

qsort_uv4 :: [Int] -> [Int]
qsort_uv4 lst = fromU (qsort4 (toU lst))

qsort_uv5 :: [Int] -> [Int]
qsort_uv5 lst = fromU (qsort5 (toU lst))


qsort1 :: UArr Int -> UArr Int
qsort1 arr | lengthU arr < 2 = arr
          | otherwise = let 
    (pivotArr,arr') = splitAtU 1 arr
    pivot = headU pivotArr
    smaller  = mapU (< pivot) arr'
    bigger   = mapU not smaller
    smaller' = packU arr' smaller
    bigger'  = packU arr' bigger
 in concatU [(qsort1 smaller'),(singletonU pivot),(qsort1 bigger')]

qsort2 :: UArr Int -> UArr Int
qsort2 arr = apply Intro.sort arr

qsort3 :: UArr Int -> UArr Int
qsort3 arr = apply Merge.sort arr

qsort4 :: UArr Int -> UArr Int
qsort4 arr = apply Radix.sort arr

qsort5 :: UArr Int -> UArr Int
qsort5 arr = apply TriHeap.sort arr

