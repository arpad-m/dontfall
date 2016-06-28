module Render exposing (renderScene)

import Element exposing (..)
import Collage exposing (..)
import Color

import BaseTypes exposing (..)

platformColor : Color.Color
platformColor = Color.rgb 30 19 67

plFill : Shape -> Form
plFill = (filled platformColor)

platformModel : Form
platformModel = group [plFill (rect 60 16), move (-30, 0) (plFill (circle 8)), move (30, 0) (plFill (circle 8))]

playerColor : Color.Color
playerColor = Color.rgb 0 128 0

playerModel : Form
playerModel = filled playerColor (rect 40 50)

renderScene : Int -> Int -> GameData -> Element
renderScene width height d = collage width height [move (d.characterPosX - (toFloat width / 2), toFloat -height/ 2) playerModel]