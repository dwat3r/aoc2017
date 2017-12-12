module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Debug.Trace

s = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]

newSTUArray :: (Int, Int) -> [Int] -> ST o (STUArray o Int Int)
newSTUArray = newListArray

redis i 0 mem = return ()
redis i m mem = do
  (min, max) <- getBounds mem
  v <- readArray mem i
  writeArray mem i (v+1)
  redis ((i + 1) `mod` max) (m - 1) mem

maxInd i ind m mem = do 
   (min, max) <- getBounds mem
   if i > max then return (ind,m)
   else do
    v <- readArray mem i
    if m < v then maxInd (i+1) i v mem
    else maxInd (i+1) ind m mem

part1 = runST $ do
  mem <- newSTUArray (0, length s) s
  go 0 S.empty mem

go :: Integer -> S.Set [Int] -> STUArray o Int Int -> ST o Integer
go i ss mem = do
  omem <- getElems mem
  traceShowM omem
  if omem `S.member` ss then return i
  else do
    (ind,m) <- maxInd 0 0 0 mem
    writeArray mem ind 0
    (min, max) <- getBounds mem
    redis ((ind + 1) `mod` max) m mem
    go (i+1) (omem `S.insert` ss) mem

-- almost same
part2 = runST $ do
  mem <- newSTUArray (0, length s) s
  go' 0 M.empty mem

go' :: Int -> M.Map [Int] Int -> STUArray o Int Int -> ST o Int
go' i ss mem = do
  omem <- getElems mem 
  case omem `M.member` ss of
    True  -> return $ i - (ss M.! omem)
    False -> do
      (ind,m) <- maxInd 0 0 0 mem
      writeArray mem ind 0
      (min, max) <- getBounds mem
      redis ((ind + 1) `mod` max) m mem
      go' (i+1) (M.insert omem i ss) mem


main = print part2