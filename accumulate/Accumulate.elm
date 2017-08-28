module Accumulate exposing (..)

import List


accumulate : (a -> b) -> List a -> List b
accumulate fn collection =
    List.map fn collection
