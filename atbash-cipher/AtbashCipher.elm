module AtbashCipher exposing (..)

import Array exposing (Array)
import Debug exposing (log)
import Dict exposing (Dict)
import Dict.Extra exposing (invert)
import List.Extra exposing (greedyGroupsOf)


cipher : Dict Char Char
cipher =
    Dict.fromList
        [ ( 'a', 'z' )
        , ( 'b', 'y' )
        , ( 'c', 'x' )
        , ( 'd', 'w' )
        , ( 'e', 'v' )
        , ( 'f', 'u' )
        , ( 'g', 't' )
        , ( 'h', 's' )
        , ( 'i', 'r' )
        , ( 'j', 'q' )
        , ( 'k', 'p' )
        , ( 'l', 'o' )
        , ( 'm', 'n' )
        , ( 'n', 'm' )
        , ( 'o', 'l' )
        , ( 'p', 'k' )
        , ( 'q', 'j' )
        , ( 'r', 'i' )
        , ( 's', 'h' )
        , ( 't', 'g' )
        , ( 'u', 'f' )
        , ( 'v', 'e' )
        , ( 'w', 'd' )
        , ( 'x', 'c' )
        , ( 'y', 'b' )
        , ( 'z', 'a' )
        ]


encode : String -> String
encode str =
    let
        clean =
            String.toLower str

        chars =
            List.foldr
                (\a b ->
                    case Dict.get a cipher of
                        Just v ->
                            v :: b

                        Nothing ->
                            b
                )
                []
                (String.toList clean)

        grouped =
            greedyGroupsOf 5 chars
                |> log "groups"
    in
        List.foldr
            (\a b ->
                String.fromList a ++ " " ++ b
            )
            ""
            grouped
            |> String.trim


decode : String -> String
decode str =
    let
        clean =
            String.toLower str

        chars =
            List.foldr
                (\a b ->
                    case Dict.get a (invert cipher) of
                        Just v ->
                            v :: b

                        Nothing ->
                            b
                )
                []
                (String.toList str)
    in
        String.fromList chars
