module RunLengthEncoding exposing (..)

import List.Extra exposing (unique, takeWhile, uncons)
import Debug exposing (log)
import Regex


version : Int
version =
    2


dedupe list acc =
    case list of
        a :: b :: rest ->
            if rest == [] then
                -- handle last pair
                if a == b then
                    acc ++ [ a ]
                else
                    acc ++ [ a, b ]
            else if a == b then
                dedupe (b :: rest) acc
            else
                dedupe (b :: rest) (acc ++ [ a ])

        _ ->
            acc


getCharCts list dd acc =
    case dd of
        head :: rest ->
            let
                len =
                    List.length (takeWhile (\char -> char == head) list)

                newAcc =
                    acc ++ [ len ]

                newList =
                    List.drop len list
            in
                getCharCts newList rest newAcc

        _ ->
            acc


getEncodeStr cts dd =
    List.map2
        (\a b ->
            if a > 1 then
                String.concat [ toString a, String.fromList [ b ] ]
            else
                String.fromList [ b ]
        )
        cts
        dd
        |> String.concat


encode str =
    let
        list =
            String.toList str

        dd =
            dedupe list []

        cts =
            getCharCts list dd []
    in
        getEncodeStr cts dd


decoder matches acc =
    case matches of
        a :: b :: rest ->
            case String.toInt a.match of
                Ok n ->
                    decoder rest (acc ++ (String.repeat n b.match))

                Err msg ->
                    decoder (b :: rest) (acc ++ a.match)

        [ a ] ->
            acc ++ a.match

        [] ->
            acc


decode str =
    let
        matches =
            Regex.find Regex.All (Regex.regex "(\\d+)|(\\D)") str
    in
        decoder matches ""
