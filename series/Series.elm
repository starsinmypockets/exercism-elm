module Series exposing (..)

import Debug exposing (..)


combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


getSlices : Int -> List Int -> List (List Int) -> List (List Int)
getSlices n list acc =
    if n <= List.length list then
        case list of
            a :: rest ->
                getSlices n rest ((List.take n list) :: acc)

            _ ->
                []
    else
        List.reverse acc


slices n ns =
    let
        list =
            List.map (String.fromChar >> String.toInt) (String.toList ns)
    in
        if n < 1 then
            Err ("Invalid size: " ++ toString n)
        else
            case combine list of
                Ok list ->
                    Ok (getSlices n list [])

                Err msg ->
                    Err msg
