module SpaceAge exposing (..)

import Debug exposing (log)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


earthYear : Float
earthYear =
    31557600


planetYearFactor : Planet -> Float
planetYearFactor p =
    case p of
        Mercury ->
            0.2408467

        Venus ->
            0.61519726

        Earth ->
            1

        Mars ->
            1.8808158

        Jupiter ->
            11.862615

        Saturn ->
            29.447498

        Uranus ->
            84.016846

        Neptune ->
            164.79132


ageOn : Planet -> Float -> Float
ageOn p age =
    age / (earthYear * (planetYearFactor p))
