module Anagram exposing (..)


agEq : List Char -> String -> Bool
agEq a b =
    if String.toList (String.toLower b) == a then
        False
    else
        List.sort (String.toList (String.toLower b)) == List.sort a


detect : String -> List String -> List String
detect word agList =
    let
        wordCharList =
            String.toList (String.toLower word)
    in
        List.filter (agEq wordCharList) agList
