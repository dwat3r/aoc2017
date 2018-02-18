import qualified Data.Map as M

data Instr = Instr String Int String String Int
    deriving Show

main = do
    input <- readFile "day8.input.txt"
    print $ part2 (parse input)

parse :: String -> [Instr]
parse s = map (go . words) $ lines s
    where
        go [r,mn,i,_,r2,c,i2] = case mn of 
            "inc" -> Instr r (read i::Int) r2 c (read i2::Int)
            "dec" -> Instr r (-1 * (read i::Int)) r2 c (read i2::Int)
            x -> error x 

eval rs r c i = case c of
    "<"  -> M.findWithDefault 0 r rs < i
    "<=" -> M.findWithDefault 0 r rs <= i
    "==" -> M.findWithDefault 0 r rs == i
    "!=" -> M.findWithDefault 0 r rs /= i
    ">=" -> M.findWithDefault 0 r rs >= i
    ">"  -> M.findWithDefault 0 r rs > i
    x    -> error x
            

part1:: [Instr] -> Int
part1 is = maximum $ M.elems $ foldl go M.empty is
    where
        go rs (Instr r i1 r2 c i2) = case eval rs r2 c i2 of
            True  -> M.insertWith (+) r i1 rs
            False -> rs 


part2:: [Instr] -> Int
part2 is = fst $ foldl go (0,M.empty) is
    where
        go (max, rs) (Instr r i1 r2 c i2) = case eval rs r2 c i2 of
            True  -> case max < rv of
                True -> (rv, M.insertWith (+) r i1 rs)
                False -> (max, M.insertWith (+) r i1 rs)
            False -> (max, rs) 
            where rv = M.findWithDefault 0 r rs + i1
            
