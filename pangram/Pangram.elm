module Pangram exposing (..)

import String


reduce : Char -> List Char -> List Char
reduce letter hits =
    let
        alph =
            [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]
    in
        if (List.member letter alph == True) && (List.member letter hits == False) then
            letter :: hits
        else
            hits


isPangram : String -> Bool
isPangram sentence =
    let
        sentArr =
            String.toList (String.toLower sentence)

        hits =
            List.foldl reduce [] sentArr
    in
        if List.length hits < 26 then
            False
        else
            True
