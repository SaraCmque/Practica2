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
