module GradeSchool exposing (addStudent, studentsInGrade, allStudents, empty)

import Dict
import Debug exposing (log)


type alias School =
    List ( Int, List String )


empty : List a
empty =
    []


addStudent : Int -> String -> School -> School
addStudent grade name sch =
    let
        school =
            Dict.fromList sch

        updated =
            Dict.update
                grade
                (\gr ->
                    case gr of
                        Just v ->
                            Just (name :: v)

                        Nothing ->
                            Just [ name ]
                )
                school
    in
        Dict.toList updated


studentsInGrade : Int -> School -> List String
studentsInGrade grade sch =
    let
        school =
            Dict.fromList sch
    in
        case Dict.get grade school of
            Just v ->
                List.sort v

            Nothing ->
                []


allStudents : a -> a
allStudents sch =
    sch
