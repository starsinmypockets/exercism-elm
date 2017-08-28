module NucleotideCount exposing (..)


version : Int
version =
    2


counter list cts =
    case list of
        a :: rest ->
            case a of
                'A' ->
                    counter rest { cts | a = cts.a + 1 }

                'C' ->
                    counter rest { cts | c = cts.c + 1 }

                'T' ->
                    counter rest { cts | t = cts.t + 1 }

                'G' ->
                    counter rest { cts | g = cts.g + 1 }

                _ ->
                    cts

        [] ->
            cts


nucleotideCounts str =
    counter (String.toList str) { a = 0, t = 0, c = 0, g = 0 }
