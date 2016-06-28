module Render exposing (..)

import Element exposing (..)
import Collage exposing (..)
import Color

import BaseTypes exposing (..)

b : Color.Color
b = Color.rgb 67 76 67

renderScene : GameData -> Element
renderScene d = collage 300 600 [filled b (rect 40 50)]