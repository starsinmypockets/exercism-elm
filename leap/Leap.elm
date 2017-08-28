module Leap exposing (..)

-- on every year that is evenly divisible by 4
-- except every year that is evenly divisible by 100
-- unless the year is also evenly divisible by 400


isLeapYear yr =
    if yr % 4 == 0 && (yr % 100 /= 0 || (yr % 400 == 0 && yr % 100 == 0)) then
        True
    else
        False
