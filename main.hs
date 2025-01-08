import Data.List 

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
    print $ matrMult m1 m2
    putStrLn "Hello"