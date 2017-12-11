module Main where

import qualified Data.HashSet as S
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Word
import Data.Bits
import Debug.Trace

s = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]

newSTUArray :: (Int, Int) -> [Int] -> ST o (STUArray o Int Int)
newSTUArray = newListArray

part1 = runST $ do
  mem <- newSTUArray (0, length s) s
  go 0 S.empty mem


go :: Integer -> S.HashSet Integer -> STUArray o Int Int -> ST o Integer
go i ss mem = do
  omem <- getElems mem 
  if toNum omem `S.member` ss then return i
  else do
    (ind,m) <- maxInd 0 0 0 mem
    redis (ind + 1) m mem
    traceShowM i
    go (i+1) (toNum omem `S.insert` ss) mem
    
redis i 0 mem = return ()
redis i m mem = do
  (min, max) <- getBounds mem
  v <- readArray mem i
  writeArray mem i (v+1)
  redis ((i + 1) `mod` max) (m - 1) mem

maxInd i ind m mem = do 
   (min, max) <- getBounds mem
   if i == max - 1 then return (ind,m)
   else do
    v <- readArray mem i
    if m < v then maxInd (i+1) i v mem
    else maxInd (i+1) ind m mem

toNum :: [Int] -> Word64
toNum = fst . foldr (\x (acc,order) -> (acc .|. order*(fromIntegral x), order `shift` 2)) (0,1) 

main = print part1