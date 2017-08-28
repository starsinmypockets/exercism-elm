module DifferenceOfSquares exposing (..)


squareOfSum : Int -> Int
squareOfSum n =
    (List.sum (List.range 0 n)) ^ 2


sumOfSquares : Int -> Int
sumOfSquares n =
    List.foldl (\x y -> y + (x ^ 2)) 0 (List.range 0 n)


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n
