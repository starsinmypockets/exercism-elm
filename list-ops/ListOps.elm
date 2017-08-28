module ListOps exposing (..)

-- honestly i'll take a pass of this


length : List a -> Int
length list =
    List.foldl (\_ a -> a + 1) 0 list


reverse : List a -> List a
reverse list =
    List.reverse list


map : (a -> b) -> List a -> List b
map fn list =
    List.map fn list


filter : (a -> Bool) -> List a -> List a
filter fn list =
    List.filter fn list


foldl : (a -> b -> b) -> b -> List a -> b
foldl fn acc list =
    List.foldl fn acc list


foldr : (a -> b -> b) -> b -> List a -> b
foldr fn acc list =
    List.foldl fn acc (List.reverse list)


append : List a -> List a -> List a
append ll list =
    List.append ll list


concat : List (List a) -> List a
concat list =
    List.concat list
