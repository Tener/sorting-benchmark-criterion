module QsortVector ( qsort_v2, qsort_v3, qsort_v4, qsort_v5 ) where

import Data.Vector
import Data.Vector.Algorithms.Combinators ( apply )
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Radix as Radix
import qualified Data.Vector.Algorithms.TriHeap as TriHeap

import Prelude hiding ( length ) 

type UArr = Vector

fromU = toList
toU = fromList

qsort_v2 :: [Int] -> [Int]
qsort_v2 lst = fromU (qsort2 (toU lst))

qsort_v3 :: [Int] -> [Int]
qsort_v3 lst = fromU (qsort3 (toU lst))

qsort_v4 :: [Int] -> [Int]
qsort_v4 lst = fromU (qsort4 (toU lst))

qsort_v5 :: [Int] -> [Int]
qsort_v5 lst = fromU (qsort5 (toU lst))


qsort2 :: UArr Int -> UArr Int
qsort2 arr = apply Intro.sort arr

qsort3 :: UArr Int -> UArr Int
qsort3 arr = apply Merge.sort arr

qsort4 :: UArr Int -> UArr Int
qsort4 arr = apply Radix.sort arr

qsort5 :: UArr Int -> UArr Int
qsort5 arr = apply TriHeap.sort arr

