import Data.List 

{- 
P_{rs} ero
swap the rows r and s

A_{rs}(lambda) ero
add (lambda) times the r row to the s row

M_{r}(lambda) ero
multiply the rth row by lambda
 -}

eroPrs :: [[Int]] -> Int -> Int -> [[Int]]
eroPrs m r s =
    let ls = [0.. (r' - 1)] ++ [s'] ++ [(r' + 1).. (s' - 1)] ++ [r']
    in [m !! i | i <- ls]
    where
        s' = s - 1  -- this is the s row number
        r' = r - 1  -- this is the row number

-- have the same rows up until s - 1
-- add lambda * rth row to sth row

eroArs :: [[Int]] -> Int -> Int -> Int -> [[Int]]
eroArs m r s lambda =
    [m !! i | i <- [0.. (s - 1)]] ++ [addrow] ++ [m !! i | i <- [(s + 1).. (length m - 1)]] -- gets the rows and inserts the new row into the list
    where
        modrow = map (* lambda) (m !! r)    -- multiplies the rth row by lambda
        addrow = zipWith (+) (m !! s) modrow    -- adds the rth row to the sth row

eroMr :: [[Int]] -> Int -> Int -> [[Int]]
eroMr m r lambda =
    [if i == (r - 1) then map (* lambda) row else row | (i, row) <- zip [0..] m]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum $ zipWith (*) x y

matrMult :: [[Int]] -> [[Int]] -> [[Int]]
matrMult x y =
    let yT = transpose y
    in [[dotProd row col | col <- yT] | row <- x]

identity :: Int -> [[Int]]
identity x = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

main :: IO ()
main = do
    let m1 = [[1, 0, 0], [0, 1, 2], [0, 0, 1]]
    let m2 = [[1, 3, 0], [0, 1, 2], [0, 0, 1]]
    print $ m1
    print $ eroPrs m1 2 3
    print $ m1
    print $ eroArs m1 1 2 3
    print $ m1
    print $ eroMr m1 1 2