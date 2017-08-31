module LargestSeriesProduct exposing (largestProduct)

import List.Extra


combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


numToList : Int -> List Int
numToList n =
    let
        strs =
            String.toList <| toString n

        maybeNs =
            List.map (String.fromChar >> String.toInt) strs

        combined =
            combine maybeNs
    in
        case combined of
            Ok ns ->
                ns

            Err x ->
                []


allZeros : List Int -> Bool
allZeros list =
    let
        notZero =
            List.filter (\a -> a == 0) list
    in
        List.length notZero == List.length list


getLargestProduct : Int -> List Int -> Int -> Int
getLargestProduct n list largest =
    if n > (List.length list) then
        largest
    else
        let
            ll =
                List.take n list

            prod =
                List.foldl (*) 1 ll
        in
            if prod > largest then
                getLargestProduct n (List.drop 1 list) prod
            else
                getLargestProduct n (List.drop 1 list) largest


largestProduct : Int -> String -> Maybe Int
largestProduct n numStr =
    let
        ints =
            String.toInt numStr
    in
        case ints of
            Ok v ->
                if n == 0 then
                    Just 1
                else if n < 0 then
                    Nothing
                else if allZeros (numToList v) then
                    Just 0
                else if n > (List.length (numToList v)) then
                    Nothing
                else if v < 0 then
                    Nothing
                else
                    Just (getLargestProduct n (numToList v) 0)

            Err msg ->
                if n == 0 then
                    Just 1
                else
                    Nothing
