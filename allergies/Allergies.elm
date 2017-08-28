module Allergies exposing (..)

import Dict
import Tuple
import Bitwise
import Debug exposing (log)


scores : Dict.Dict String Int
scores =
    Dict.fromList
        [ ( "eggs", 1 )
        , ( "peanuts", 2 )
        , ( "shellfish", 4 )
        , ( "strawberries", 8 )
        , ( "tomatoes", 16 )
        , ( "chocolate", 32 )
        , ( "pollen", 64 )
        , ( "cats", 128 )
        ]


isAllergicTo : String -> Int -> Bool
isAllergicTo item n =
    Dict.get item scores
        |> Maybe.withDefault 0
        |> Bitwise.and n
        |> (/=) 0


hasAllergy n scores acc =
    Dict.filter (\k v -> v >= n) scores
        |> Dict.keys



-- add score to acc
-- remove from score
-- repeat


toList : Int -> List String
toList n =
    Dict.filter (\k v -> isAllergicTo k n) scores
        |> Dict.keys
