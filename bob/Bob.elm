module Bob exposing (..)

import Regex exposing (..)


t =
    String.trim


hey str =
    let
        tStr =
            String.trim str
    in
        if contains (regex "[/?]$") tStr && contains (regex "[A-Z]{4}") (t str) then
            "Whoa, chill out!"
        else if contains (regex "[0-9].") tStr && contains (regex "!") (t str) then
            "Whoa, chill out!"
        else if contains (regex "[/?]$") tStr then
            "Sure."
        else if contains (regex "[A-Z]{4}") tStr then
            "Whoa, chill out!"
        else if contains (regex "^$") tStr then
            "Fine. Be that way!"
        else
            "Whatever."
