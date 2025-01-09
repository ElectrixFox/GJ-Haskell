import Data.List 

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum $ zipWith (*) x y

matrMult :: [[Int]] -> [[Int]] -> [[Int]]
matrMult x y =
    let yT = transpose y
    in [[dotProd row col | col <- yT] | row <- x]

eroPrs :: [[Int]] -> Int -> Int -> [[Int]]
eroPrs m r s =
    let ls = [0.. (r' - 1)] ++ [s'] ++ [(r' + 1).. (s' - 1)] ++ [r']
    in [m !! i | i <- ls]
    where
        s' = s - 1
        r' = r - 1

eroArs :: [[Int]] -> Int -> Int -> Int -> [[Int]]
eroArs m r s lambda =
    [if i == s' then zipWith (+) row (map (* lambda) (m !! r'))
    else row | (i, row) <- zip [0..] m]
    where
        s' = s - 1
        r' = r - 1

eroMr :: [[Int]] -> Int -> Int -> [[Int]]
eroMr m r lambda =
    [if i == r' then map (* lambda) row else row | (i, row) <- zip [0..] m]
    where
        r' = r - 1


mullist :: [Int] -> Int -> [Int]
mullist x l = map (* l) x

identity :: Int -> [[Int]]
identity x = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

main :: IO ()
main = do
    let m1 = [[1, 0, 0], [0, 1, 2], [0, 0, 1]]
    let m2 = [[1, 3, 0], [0, 1, 2], [0, 0, 1]]
    print $ matrMult m1 m2
    putStrLn ""
    print $ m1
    print $ eroPrs m1 2 3
    putStrLn ""
    print $ m1
    print $ eroArs m1 2 3 2
    putStrLn ""
    print $ m1
    print $ eroMr m1 2 2
    putStrLn "Hello"