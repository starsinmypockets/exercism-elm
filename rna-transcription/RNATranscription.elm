module RNATranscription exposing (..)

import Dict exposing (Dict)
import Char
import Debug exposing (log)
import Regex


rna : Char -> Result Char Char
rna dna =
    case dna of
        'C' ->
            Ok 'G'

        'G' ->
            Ok 'C'

        'T' ->
            Ok 'A'

        'A' ->
            Ok 'U'

        _ ->
            Err dna


toRNA dna =
    case String.uncons dna of
        Just ( head, rest ) ->
            case rna head of
                Ok result ->
                    Result.map (String.cons result) (toRNA rest)

                Err err ->
                    Err err

        Nothing ->
            Ok ""
