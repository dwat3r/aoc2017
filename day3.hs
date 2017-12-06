import qualified Data.Map as M

import Debug.Trace

gen = concat $ zipWith replicate (concatMap (\x -> [x,x]) [1..]) $ cycle [r,u,l,d]
r (x,y) = (x+1,y)
u (x,y) = (x,y+1)
l (x,y) = (x-1,y)
d (x,y) = (x,y-1)

part1 n = -1 + go n (0,0) gen
  where
    go 0 (x,y) _ = abs x + abs y
    go n c (d:ds) = go (n-1) (d c) ds


part2 n = go ((0,0),1) (M.singleton (0,0) 1) gen
  where
    go (c,i) m (dir:dirs) | i > n = [(c,i)]
                          | True  = (c,i):go (dir c, v) (M.insert (dir c) v m) dirs
      where
        v = sum $ map (\d' -> M.findWithDefault 0 (d' $ dir c) m) [r, r . u, u, l . u, l, l . d, d, r . d]