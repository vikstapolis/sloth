{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

-- Wraps the Data.Vector.Mutable interface,
-- providing automatic resizing 
--
-- Uses IOVector

module DataStructs.Vector (
    Vector,
    length, null,
    init, tail, take, drop, slice, splitAt,
    (!),
    write, append, concat,
    fromList
) where

import           Prelude hiding (length, null, init, tail, take, drop, splitAt, concat)
import qualified Prelude as P

import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

import Data.Coerce
import Control.Monad (forM_)

data Vector a = Vector !Int !(IOVector a)

growSize :: Int -> Int
growSize n = (n * 3) `div` 2

-- Reimplementing basic Vector operations
length :: Vector a -> Int
length (Vector len _) = len

null :: Vector a -> Bool
null (Vector len _) = len == 0

slice :: Int -> Int -> Vector a -> Vector a
slice i n (Vector len v) = Vector n (V.slice i n v)

init, tail :: Vector a -> Vector a
init (Vector len v) = Vector (len-1) (V.init v)
tail (Vector len v) = Vector (len-1) (V.tail v)

take, drop :: Int -> Vector a -> Vector a
take n vec@(Vector len v)
    | n < 0 = drop (len + n) vec
    | otherwise = Vector n (V.take n v)
drop n vec@(Vector len v)
    | n < 0 = take (len + n) vec
    | otherwise = Vector n (V.drop n v)

splitAt  :: Int -> Vector a -> (Vector a, Vector a)
splitAt n (Vector len v) = case V.splitAt n v of (a,b) -> (Vector n a, Vector (len-n) b)

-- Reading, writing, creating and printing vectors
(!) :: Vector a -> Int -> IO a
(Vector _ v) ! i = V.read v i

write :: Int -> a -> Vector a -> IO ()
write i x (Vector len v) = V.write v i x

append :: a -> Vector a -> IO (Vector a)
append x (Vector len v) = do
    v' <- if len < V.length v
              then return v
              else V.unsafeGrow v (growSize len - len)
    V.write v' len x
    return $ Vector (len+1) v'

insert :: Int -> a -> Vector a -> IO (Vector a)
insert i x (Vector len v) = do
    v' <- ensureCapacity (len+1) v

    forM_ [len-1,len-2 .. i] $ \idx -> do
        val <- V.read v' idx
        V.write v' (idx+1) val

    V.write v' i x
    return $ Vector (len+1) v'

concat :: Vector a -> Vector a -> IO (Vector a)
concat (Vector len1 v1) (Vector len2 v2) = do
    v <- ensureCapacity (len1+len2) v1

    forM_ [0 .. len2 - 1] $ \idx -> do
        V.read v2 idx >>= V.write v (len1+idx)

    return $ Vector (len1+len2) v

ensureCapacity :: Int -> IOVector a -> IO (IOVector a)
ensureCapacity n v
    | n <= len  = return v
    | otherwise =  let newLen = max (len+n) (growSize len)
                   in V.unsafeGrow v (newLen-len)
    where
        len = V.length v


fromList :: [a] -> IO (Vector a)
fromList xs = do
    let len = P.length xs
    vec <- V.new len
    forM_ (zip [0..] xs) (uncurry $ V.write vec)
    return $ Vector len vec

printVec :: Show a => Vector a -> IO ()
printVec (Vector len v) = do
    putChar '['
    V.read v 0 >>= putStr . show
    V.mapM_ (putStr . (',':) . show) (V.take (len-1) $ V.tail v)
    putStrLn "]"

func = do
    vec <- fromList [1,2,3]
    printVec vec
    vec' <- append 4 vec
    printVec vec'
    print (length vec')
    vec'' <- append 5 vec'
    printVec vec''
    print (length vec'')
    write 2 7 vec''
    printVec vec''
