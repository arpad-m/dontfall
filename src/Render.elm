module Render exposing (renderScene)

import Element exposing (..)
import Collage exposing (..)
import Color

import BaseTypes exposing (..)

b : Color.Color
b = Color.rgb 67 76 67

playerModel : Form
playerModel = filled b (rect 40 50)

renderScene : Int -> Int -> GameData -> Element
renderScene width height d = collage width height [move (d.characterPosX - (toFloat width / 2), 0) playerModel]