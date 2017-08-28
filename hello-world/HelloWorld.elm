module HelloWorld exposing (..)

-- It's good style to include any types at the top level of your modules.


helloWorld : Maybe String -> String
helloWorld name =
    case name of
        Just n ->
            "Hello, " ++ n ++ "!"

        Nothing ->
            "Hello, World!"
