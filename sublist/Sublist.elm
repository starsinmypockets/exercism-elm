module Sublist exposing (..)


version : Int
version =
    2


type ListComparison
    = Equal
    | Unequal
    | Sublist
    | Superlist


listAtStart a b =
    if List.take (List.length a) b == a then
        True
    else
        False


has : List a -> List a -> Bool
has alist blist =
    if listAtStart alist blist then
        True
    else
        case blist of
            a :: rest ->
                if listAtStart alist rest then
                    True
                else
                    has alist rest

            [] ->
                False


sublist a b =
    if a == b then
        Equal
    else if has a b then
        Sublist
    else if has b a then
        Superlist
    else
        Unequal
