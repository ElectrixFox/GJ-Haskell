import Data.List 

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum $ zipWith (*) x y

matrMult :: [[Int]] -> [[Int]] -> [[Int]]
matrMult x y =
    let yT = transpose y
    in [[dotProd row col | col <- yT] | row <- x]

{-
for i in range m
    if i == r
        M += m[s]
    else if i == s
        M += m[r]
    else
        M += m[i]

-}

eroPrs :: [[Int]] -> Int -> Int -> [[Int]]
eroPrs m r s =
    let ls = [0.. (r - 1 - 1)] ++ [s - 1] ++ [(r + 1 - 1).. (s - 1 - 1)] ++ [r - 1]
    in [m !! i | i <- ls]

test :: [[Int]] -> Int -> Int -> [Int]
test m r s = [1.. (r - 1)] ++ [s] ++ [(r + 1).. (s - 1)] ++ [r]

identity :: Int -> [[Int]]
identity x = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

main :: IO ()
main = do
    let m1 = [[1, 0, 0], [0, 1, 2], [0, 0, 1]]
    let m2 = [[1, 3, 0], [0, 1, 2], [0, 0, 1]]
    print $ matrMult m1 m2
    print $ eroPrs m1 2 3
    putStrLn "Hello"