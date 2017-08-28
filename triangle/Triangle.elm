module Triangle exposing (..)

import List
import List.Extra exposing (unique)


type Triangle
    = Equilateral
    | Scalene
    | Isosceles


version : Int
version =
    2


triangleKind x y z =
    if x <= 0 || y <= 0 || z <= 0 then
        (Err "Invalid lengths")
    else if (x + y < z) || (x + z < y) || (y + z < x) then
        (Err "Violates inequality")
    else if x <= 0 || y <= 0 || z <= 0 then
        (Err "Invalid lengths")
    else if x == y && y == z then
        (Ok Equilateral)
    else if List.length (unique [ x, y, z ]) == 3 then
        (Ok Scalene)
    else if List.length (unique [ x, y, z ]) >= 2 then
        (Ok Isosceles)
    else
        (Err "Missing Case")
