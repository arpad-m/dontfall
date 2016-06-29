module Background exposing (background)

import Element exposing (..)
import Collage exposing (..)
import Color

import Random exposing (..)

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
    (let (l, _) = step (Random.list (round <| width * height / 9000) (generateBgCircle width height)) (initialSeed 2356) in l))
