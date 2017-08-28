module ScrabbleScore exposing (..)

import Dict


scores =
    Dict.fromList
        [ ( [ 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T' ], 1 )
        , ( [ 'D', 'G' ], 2 )
        , ( [ 'B', 'C', 'M', 'P' ], 3 )
        , ( [ 'F', 'H', 'V', 'W', 'Y' ], 4 )
        , ( [ 'K' ], 5 )
        , ( [ 'J', 'X' ], 8 )
        , ( [ 'Q', 'Z' ], 10 )
        ]


charInKey : Char -> List Char -> Bool
charInKey a charArr =
    List.member a charArr


lookup : Char -> Int
lookup char =
    -- thanks to @jessta from Elm slack for more succinct syntax
    Dict.keys scores
        |> List.filter (charInKey char)
        |> List.head
        |> Maybe.andThen (\x -> Dict.get x scores)
        |> Maybe.withDefault 0


reduce : Char -> Int -> Int
reduce char total =
    total + lookup char


scoreWord : String -> Int
scoreWord word =
    let
        wordList =
            String.toUpper word
                |> String.toList
    in
        if List.length wordList == 0 then
            0
        else
            -- (a -> b -> b) -> b -> List a -> b
            List.foldl reduce 0 wordList
