module RunLengthEncoding exposing (..)

import List.Extra exposing (unique, takeWhile, uncons)
import Debug exposing (log)


version : Int
version =
    2



-- decompose list into multiple lists of equal character


decomp : List Char -> Char -> List (List Char) -> List (List Char)
decomp list un acc =
    case uncons un of
        Just ( head, rest ) ->
            let
                taken =
                    (takeWhile (\x -> x == head) list) :: acc

                untaken =
                    List.drop (List.length taken)
            in
                decomp
                    untaken
                    rest
                    (taken :: acc)

        Nothing ->
            acc


encode str =
    let
        chars =
            String.toList str

        un =
            unique chars
    in
        decomp chars un []
            |> List.concat
            |> String.fromList


decode str =
    "WWWWWWWWWW"
