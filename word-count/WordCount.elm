module WordCount exposing (..)

import Dict exposing (Dict)
import Debug exposing (log)
import Regex exposing (regex)


addWord : String -> Dict String Int -> Dict String Int
addWord w acc =
    let
        v =
            Dict.get w acc
                |> Maybe.withDefault 0
    in
        Dict.insert w (v + 1) acc


wordCount : String -> Dict String Int
wordCount phrase =
    let
        words =
            String.toLower phrase
                |> Regex.replace Regex.All (regex "[^a-zA-Z0-9\\s]") (\_ -> "")
                |> String.split " "
                |> List.filter (\w -> w /= "")
    in
        List.foldl addWord Dict.empty words
