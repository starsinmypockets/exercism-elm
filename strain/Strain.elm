module Strain exposing (..)


keep fn list =
    List.filter fn list


discard fn list =
    List.partition fn list
        |> Tuple.second
