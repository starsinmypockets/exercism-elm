module Say exposing (say, SayError(..))

import Dict
import List.Extra


type SayError
    = Negative
    | TooLarge


valid n =
    True


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


getDecimal : Int -> String
getDecimal n =
    Maybe.withDefault "!d" (Dict.get n decimal)


decimal : Dict.Dict Int String
decimal =
    Dict.fromList
        [ ( 0, "zero" )
        , ( 1, "one" )
        , ( 2, "two" )
        , ( 3, "three" )
        , ( 4, "four" )
        , ( 5, "five" )
        , ( 6, "six" )
        , ( 7, "seven" )
        , ( 8, "eight" )
        , ( 9, "nine" )
        , ( 10, "ten" )
        ]


teenish : Dict.Dict Int String
teenish =
    Dict.fromList
        [ ( 11, "eleven" )
        , ( 12, "twelve" )
        , ( 13, "thirteen" )
        , ( 14, "fourteen" )
        , ( 15, "fifteen" )
        , ( 16, "sixteen" )
        , ( 17, "seventeen" )
        , ( 18, "eighteen" )
        , ( 19, "nineteen" )
        ]


decade : Dict.Dict Int String
decade =
    Dict.fromList
        [ ( 2, "twenty" )
        , ( 3, "thirty" )
        , ( 4, "forty" )
        , ( 5, "fifty" )
        , ( 6, "sixty" )
        , ( 7, "seventy" )
        , ( 8, "eighty" )
        , ( 9, "ninety" )
        ]


getTeenish : Int -> String
getTeenish n =
    Maybe.withDefault "!te!" (Dict.get n teenish)


getTens : Int -> Int -> String
getTens a b =
    let
        tens =
            Maybe.withDefault "!tn!" (Dict.get a decade)
    in
        if a == 1 then
            getTeenish (a * 10 + b)
        else if b == 0 then
            tens
        else
            tens ++ "-" ++ getDecimal b


getHundreds : Int -> Int -> Int -> String
getHundreds a b c =
    let
        hundreds =
            getDecimal a

        tens =
            if b == 0 && c == 0 then
                ""
            else
                getTens b c

        hasTens =
            String.length tens > 0
    in
        if hasTens then
            hundreds ++ " hundred and " ++ tens
        else
            hundreds ++ " hundred"


getPrefix : Int -> String
getPrefix n =
    if n <= 10 then
        getDecimal n
    else if n > 10 && n < 100 then
        getTens n 0
    else
        -- ugh todo get hundreds etc
        "igh"


reduce : List Int -> Int
reduce list =
    case list of
        [ a ] ->
            a

        [ a, b ] ->
            a * 10 + b

        [ a, b, c ] ->
            a * 100 + b * 10 + c

        [ a, b, c, d ] ->
            a * 1000 * b * 100 * c * 10 + d

        _ ->
            0


getThousands : Int -> Int -> Int -> Int -> String
getThousands a b c d =
    let
        thousands =
            getPrefix a

        hundreds =
            if b == 0 && c == 0 && d == 0 then
                ""
            else
                getHundreds b c d

        hasHundreds =
            String.length hundreds > 0 && hundreds /= "zero hundred"
    in
        if thousands == "zero" then
            ""
        else if hasHundreds then
            thousands ++ " thousand " ++ hundreds
        else
            thousands ++ " thousand"


getMillions : Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
getMillions a b c d e f g =
    let
        millions =
            getPrefix a

        thousands =
            getThousands (reduce [ b, c, d ]) e f g

        hasThousands =
            thousands /= "zero thousand"
    in
        if hasThousands then
            millions ++ " million " ++ thousands
        else
            millions ++ "million"


say : Int -> Result SayError String
say n =
    let
        nlist =
            numToList n
    in
        if n < 0 then
            Err Negative
        else if n <= 10 then
            Ok (getDecimal n)
        else
            case nlist of
                [ a, b ] ->
                    Ok (getTens a b)

                [ a, b, c ] ->
                    Ok (getHundreds a b c)

                [ a, b, c, d ] ->
                    Ok (getThousands a b c d)

                [ a, b, c, d, e ] ->
                    Ok (getThousands (reduce [ a, b ]) c d e)

                [ a, b, c, d, e, f ] ->
                    Ok (getThousands (reduce [ a, b, c ]) d e f)

                [ a, b, c, d, e, f, g ] ->
                    Ok (getMillions a b c d e f g)

                [ a, b, c, d, e, f, g, h ] ->
                    Ok (getMillions (reduce [ a, b ]) c d e f g h)

                [ a, b, c, d, e, f, g, h, i ] ->
                    Ok (getMillions (reduce [ a, b, c ]) d e f g h i)

                [ a, b, c, d, e, f, g, h, i, j ] ->
                    Ok "one billion"

                [ a, b, c, d, e, f, g, h, i, j, k, l ] ->
                    Ok "ten billion"

                [ a, b, c, d, e, f, g, h, i, j, k, l, m ] ->
                    Ok "one hundred billion"

                _ ->
                    Err TooLarge
