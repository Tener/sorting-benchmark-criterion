{-# LANGUAGE ForeignFunctionInterface #-}

module SortFFI_CPP ( sort_ints_intr, sort_ints_heap, sort_ints_stable, sort_ints_NOT ) where

import Foreign
import Foreign.StablePtr
import Foreign.C

import Control.Applicative

foreign import ccall "sort_ints_intr" sort_ints_intr_cpp :: Ptr Int -> Int -> IO ()
foreign import ccall "sort_ints_heap" sort_ints_heap_cpp :: Ptr Int -> Int -> IO ()
foreign import ccall "sort_ints_stable" sort_ints_stable_cpp :: Ptr Int -> Int -> IO ()
foreign import ccall "sort_ints_NOT" sort_ints_NOT_cpp :: Ptr Int -> Int -> IO ()

sort_ints_intr = wrap_cpp sort_ints_intr_cpp
sort_ints_heap = wrap_cpp sort_ints_heap_cpp
sort_ints_stable = wrap_cpp sort_ints_stable_cpp
sort_ints_NOT = wrap_cpp sort_ints_NOT_cpp

wrap_cpp :: (Ptr Int -> Int -> IO ()) -> [Int] -> [Int]
wrap_cpp cpp_sorter xs = unsafePerformIO $ withArrayLen xs (\len arr -> cpp_sorter arr len >> peekArray len arr)
  