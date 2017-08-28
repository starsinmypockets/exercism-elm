module Raindrops exposing (..)

import Tuple exposing (second)
import String
import Debug exposing (log)
import List.Extra exposing (unique)


raindrops : Int -> String
raindrops n =
    help ( n, [] )
        |> Tuple.second
        |> List.reverse
        |> unique
        |> String.concat


help : ( Int, List String ) -> ( Int, List String )
help ( n, drops ) =
    if (n == 0) && (List.isEmpty drops == True) then
        ( 0, (toString 0) :: drops )
    else if n % 3 == 0 then
        help ( n // 3, "Pling" :: drops )
    else if n % 5 == 0 then
        help ( n // 5, "Plang" :: drops )
    else if n % 7 == 0 then
        help ( n // 7, "Plong" :: drops )
    else if (List.isEmpty drops == False) then
        ( 0, drops )
    else
        ( 0, (toString n) :: drops )
