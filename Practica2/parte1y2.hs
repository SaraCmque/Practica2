module Parte1y2(solveRPN, condNum) where
import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:xs) "neg1" = (negate x):xs
          foldingFunction (x:xs) "raiz2" = (sqrt x):xs
          foldingFunction (x:xs) "condnumero" = (condNum x):xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs "product" = [product (xs)]
          foldingFunction xs "promedio" = (sum xs / fromIntegral (length xs)):xs
          foldingFunction xs numberString = read numberString:xs

condNum :: (RealFloat val) => val -> Float
condNum x
    | x == 3 = 100
    | x == 5 = 10
    | otherwise = 0

main = do
    putStrLn ""
    putStrLn "Welcome to Sara's RPN calculator, RPN Example: 10 4 3 + 2 * - = -4"
    putStrLn ""
    putStrLn "10 4 3 + 2 * - ="
    print(solveRPN "10 4 3 + 2 * -")
    putStrLn ""
    putStrLn "4 5 * =" 
    print(solveRPN "4 5 *")
    putStrLn ""
    putStrLn "20 10 5 / + ="
    print(solveRPN "20 10 5 / +" )
    putStrLn ""
    putStrLn "10 67 neg1 ="
    print(solveRPN "10 67 neg1")
    putStrLn ""
    putStrLn "10 67 neg1 + ="
    print(solveRPN "10 67 neg1 +")
    putStrLn ""
    putStrLn "10 67 neg1 * ="
    print(solveRPN "10 67 neg1 *")
    putStrLn ""
    putStrLn "10 16 raiz2 ="
    print(solveRPN "10 16 raiz2")
    putStrLn ""
    putStrLn "10 16 raiz2 + ="
    print(solveRPN "10 16 raiz2 +")
    putStrLn ""
    putStrLn "10 5 condnumero + ="
    print(solveRPN "10 5 condnumero +")
    putStrLn ""
    putStrLn "10 5 condnumero - ="
    print(solveRPN "10 5 condnumero -")
    putStrLn ""
    putStrLn "10 3 condnumero + ="
    print(solveRPN "10 3 condnumero +")
    putStrLn ""
    putStrLn "10 2 condnumero + ="
    print(solveRPN "10 2 condnumero +")
    putStrLn ""
    putStrLn "10 67 15 sum ="
    print(solveRPN "10 67 15 sum")
    putStrLn ""
    putStrLn "10 16 10 product ="
    print(solveRPN "10 16 10 product")
    putStrLn ""
    putStrLn "120 20 40 120 promedio ="
    print(solveRPN "120 20 40 120 promedio")


    