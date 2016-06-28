import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Html exposing (..)
import Color

b : Color.Color
b = Color.rgb 67 76 67

main : Html msg
main = toHtml (collage 300 600 [filled b (rect 40 50)])