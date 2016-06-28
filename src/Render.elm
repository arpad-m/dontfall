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

randomizeBgCircle : Float -> Float -> Seed -> (Form, Seed)
randomizeBgCircle width height seed =
    let (((xpos, ypos), csize), nseed) =
        step (pair (pair (xgen width) (xgen height)) (float 1 3)) seed
    in
        (bgCircle xpos ypos csize, nseed)

listifyHelper (y, s) = ([y], s)

backgroundColor : Color.Color
backgroundColor = Color.rgb 30 19 67

background : Float -> Float -> Form
background width height = group
    ([filled backgroundColor (rect width height)] ++
    (let (l, _) = generateRandList 20 (randomizeBgCircle width height >> listifyHelper) (initialSeed 2356) in l))

renderPlatform : Float -> Float -> (Float, Float) -> Form
renderPlatform width height (x, y) =
    move (x - width / 2, y - height / 2) platformModel

renderScene : Int -> Int -> GameData -> Element
renderScene width height d = collage width height
    (
        [ background (toFloat width) (toFloat height)
        , move (d.characterPosX - (toFloat width / 2), d.characterPosY - (toFloat height/ 2)) playerModel
        ]
        ++ List.map (renderPlatform (toFloat width) (toFloat height)) d.platforms
    )