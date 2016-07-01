module Render exposing (renderScene)

import Element exposing (..)
import Collage exposing (..)
import Text
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

pausedText : Text.Text
pausedText = Text.color Color.white (Text.fromString "Game paused. Press [P] to resume.")

renderScene : GameData -> Element
renderScene d = collage d.width d.height
    (
        [ d.background ]
        ++ List.map (renderPlatform d.flWidth d.flHeight) d.platforms
        ++ [ move (d.characterPosX - d.flWidth / 2, d.characterPosY - d.flHeight/ 2) playerModel ]
        ++ if d.paused then [ text pausedText ] else []
    )
