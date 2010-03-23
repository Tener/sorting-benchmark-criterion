{-# LANGUAGE ForeignFunctionInterface #-}

module SortFFI ( qsort_ffi ) where

import Foreign
import Foreign.StablePtr
import Foreign.C

import Control.Applicative

type CompareInt = (Ptr ()) -> (Ptr ()) -> IO Int

foreign import ccall "wrapper" mkCompareInt :: CompareInt -> IO (FunPtr CompareInt)

foreign import ccall "stdlib.h qsort" qsort_c :: Ptr () -> 
                                                 CSize -> 
                                                 CSize -> 
                                                 (FunPtr CompareInt) -> IO ()
-- ((Ptr ()) -> (Ptr ()) -> IO Int)

-- cmp_stable :: (Ord a) => a -> (Ptr ()) -> (Ptr ()) -> IO Int
-- cmp_stable dummy s1 s2 = do
--   v1 <- deRefStablePtr (castPtrToStablePtr s1)
--   v2 <- deRefStablePtr (castPtrToStablePtr s2)
--   return $ case compare (v1 `oftype` dummy) v2 of
--     LT -> (-1)
--     EQ -> 0
--     GT -> 1

cmp_int :: (Ptr ()) -> (Ptr ()) -> IO Int
cmp_int s1 s2 = do
  v1 <- deRefStablePtr (castPtrToStablePtr s1)
  v2 <- deRefStablePtr (castPtrToStablePtr s2)
  return $ case compare (v1 :: Int) (v2 :: Int) of
    LT -> (-1)
    EQ -> 0
    GT -> 1

{-# NOINLINE qsort_ffi #-}
qsort_ffi :: (Ord a) => [a] -> [a]
qsort_ffi elems = unsafePerformIO $ do
                    cmp_int_ptr <- mkCompareInt cmp_int
                    stables <- mapM newStablePtr elems
                    stableArr <- castPtr <$> (newArray stables)
                    let len = length elems
                    qsort_c stableArr (fromIntegral $ len) (fromIntegral . sizeOf $ stableArr) cmp_int_ptr
                    
                    elems <- mapM deRefStablePtr =<< peekArray len (castPtr stableArr)
                    -- clean up
                    mapM_ freeStablePtr stables
                    freeHaskellFunPtr cmp_int_ptr
                    
                    return elems
                    
  