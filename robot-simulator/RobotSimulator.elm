module RobotSimulator exposing (..)


type alias Robot =
    { bearing : Bearing, coordinates : Coordinates }


type alias Coordinates =
    { x : Int, y : Int }


type Bearing
    = North
    | South
    | East
    | West


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight robot =
    case robot.bearing of
        North ->
            { robot | bearing = East }

        South ->
            { robot | bearing = West }

        East ->
            { robot | bearing = South }

        West ->
            { robot | bearing = North }


turnLeft : Robot -> Robot
turnLeft robot =
    case robot.bearing of
        North ->
            { robot | bearing = West }

        South ->
            { robot | bearing = East }

        East ->
            { robot | bearing = North }

        West ->
            { robot | bearing = South }


advance : Robot -> Robot
advance robot =
    case robot.bearing of
        North ->
            { robot | coordinates = { x = robot.coordinates.x, y = (robot.coordinates.y + 1) } }

        South ->
            { robot | coordinates = { x = robot.coordinates.x, y = (robot.coordinates.y - 1) } }

        East ->
            { robot | coordinates = { x = (robot.coordinates.x + 1), y = robot.coordinates.y } }

        West ->
            { robot | coordinates = { x = (robot.coordinates.x - 1), y = robot.coordinates.y } }


doCommand : Char -> Robot -> Robot
doCommand ch robot =
    case ch of
        'L' ->
            turnLeft robot

        'R' ->
            turnRight robot

        'A' ->
            advance robot

        _ ->
            robot


simulate : String -> Robot -> Robot
simulate str robot =
    List.foldl doCommand robot (String.toList str)
