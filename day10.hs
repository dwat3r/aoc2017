module Main where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.Vector as V
import Debug.Trace
import Control.Monad

--s = V.fromList [14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244]
s = V.fromList [3,4,1,5]
asize = 5

newSTUArray :: (Int, Int) -> [Int] -> ST o (STUArray o Int Int)
newSTUArray = newListArray

part1 = runST $ do
    --list <- newSTUArray (0, 255) [0..255]
    list <- newSTUArray (0, 4) [0..4]
    go 0 0 s list

go lp p lens list = do
    debug <- getElems list
    traceShowM (lp,p,debug)
    if lp == length lens then do
        a <- readArray list 0
        b <- readArray list 1
        return $ a * b
    else do
        let ll = lens V.! lp
        revSubList list p ((p + ll - 1) `mod` asize) (p < (p + ll - 1) `mod` asize)
        go (lp + 1) ((p + ll + lp) `mod` asize) lens list


revSubList list a b lt =
    if (lt && (b - a) <= 2) || (not lt && (a - b) <= 2) then return list
    else do    
        av <- readArray list a
        bv <- readArray list b
        writeArray list a bv
        writeArray list b av
        revSubList list ((a+1) `mod` asize) (if b == 0 then asize - 1 else b-1) lt
    
  
main = print part1