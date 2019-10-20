module Styles exposing (em, eml, gray, red, white)

import Element exposing (Color, Length, px, rgb255)


white : Color
white =
    rgb255 255 255 255


gray : Color
gray =
    rgb255 192 192 192


red : Color
red =
    rgb255 255 0 0


em : Float -> Int
em =
    truncate << (*) 18


eml : Float -> Length
eml =
    px << em
