module PhoneNumber exposing (..)

import Regex


clean : String -> String
clean n =
    let
        matches =
            Regex.find Regex.All (Regex.regex "\\d+") n

        str =
            List.foldl (\x acc -> acc ++ x.match) "" matches
    in
        if (String.length str == 11) && String.left 1 str == "1" then
            String.dropLeft 1 str
        else
            str


getNumber : String -> Maybe String
getNumber n =
    let
        num =
            clean n
    in
        if (String.length num) /= 10 then
            Nothing
        else
            Just num


prettyPrint : String -> Maybe String
prettyPrint n =
    let
        area =
            String.slice 0 3 (clean n)

        exch =
            String.slice 3 6 (clean n)

        suff =
            String.slice 6 10 (clean n)
    in
        Just ("(" ++ area ++ ")" ++ " " ++ exch ++ "-" ++ suff)
