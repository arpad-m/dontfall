module Render exposing (renderScene)

import Element exposing (..)
import Collage exposing (..)
import Color

import BaseStuff exposing (..)
import Random exposing (..)

platformColor : Color.Color
platformColor = Color.rgb 100 19 67

plFill : Shape -> Form
plFill = (filled platformColor)

platformModel : Form
platformModel = group [plFill (rect 60 16), move (-30, 0) (plFill (circle 8)), move (30, 0) (plFill (circle 8))]

playerColor : Color.Color
playerColor = Color.rgb 0 128 0

playerModel : Form
playerModel = filled playerColor (rect 40 50)

renderPlatform : Float -> Float -> (Float, Float) -> Form
renderPlatform width height (x, y) =
    move (x - width / 2, y - height / 2) platformModel

renderScene : Int -> Int -> GameData -> Element
renderScene width height d = collage width height
    (let
        (flWidth, flHeight) = (toFloat width, toFloat height)
    in (
        [ d.background ]
        ++ List.map (renderPlatform flWidth flHeight) d.platforms
        ++ [ move (d.characterPosX - flWidth / 2, d.characterPosY - flHeight/ 2) playerModel ]
    ))
