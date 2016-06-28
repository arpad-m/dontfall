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

bgCircle : Float -> Float -> Float -> Form
bgCircle x y s = move (x, y) (outlined
    { color = Color.white
    , width = 1
    , cap = Flat
    , join = Smooth
    , dashing = []
    , dashOffset = 0
    }
    (circle s))

xgen x = (float (-x/2) (x/2))

generateBgCircle : Float -> Float -> Generator Form
generateBgCircle width height =
    map3 bgCircle (xgen width) (xgen height) (float 1 3)

backgroundColor : Color.Color
backgroundColor = Color.rgb 30 19 67

background : Float -> Float -> Form
background width height = group
    ([filled backgroundColor (rect width height)] ++
    (let (l, _) = step (Random.list 20 (generateBgCircle width height)) (initialSeed 2356) in l))

renderPlatform : Float -> Float -> (Float, Float) -> Form
renderPlatform width height (x, y) =
    move (x - width / 2, y - height / 2) platformModel

renderScene : Int -> Int -> GameData -> Element
renderScene width height d = collage width height
    (let
        (flWidth, flHeight) = (toFloat width, toFloat height)
    in (
        [ background flWidth flHeight ]
        ++ List.map (renderPlatform flWidth flHeight) d.platforms
        ++ [ move (d.characterPosX - flWidth / 2, d.characterPosY - flHeight/ 2) playerModel ]
    ))
