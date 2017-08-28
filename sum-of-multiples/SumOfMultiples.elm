module SumOfMultiples exposing (..)

import List
import List.Extra exposing (unique)
import Debug exposing (log)


hasMultiple : List Int -> Int -> Bool
hasMultiple ns n =
    let
        multiples =
            List.filter (\x -> n % x == 0) ns
    in
        List.length multiples > 0


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples ns n =
    List.filter (hasMultiple ns) (List.range 0 (n - 1))
        |> List.foldl (+) 0
