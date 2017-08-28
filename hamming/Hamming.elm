module Hamming exposing (..)

import Debug exposing (log)


countDistance : List Char -> List Char -> Int
countDistance a b =
    let
        pairs =
            List.map2 (\x y -> ( x, y )) a b
    in
        List.foldl
            (\x acc ->
                if Tuple.first x /= Tuple.second x then
                    acc + 1
                else
                    acc
            )
            0
            pairs


distance : String -> String -> Maybe Int
distance a b =
    let
        a_ =
            String.toList a

        b_ =
            String.toList b
    in
        if List.length a_ /= List.length b_ then
            Nothing
        else
            Just (countDistance a_ b_)
