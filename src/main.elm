module Main exposing (..)

import Text exposing (..)
import Html exposing (..)
import Html.App
import Html.Events
import Time
import Element exposing (..)
import Json.Decode as Decode exposing ((:=))

import BaseStuff exposing (..)
import Render exposing (..)

height = 600
width = 300

stepTime : GameData -> Time.Time -> GameData
stepTime d t = {d | time = t }

updateScene : GameMsg -> GameData -> (GameData, Cmd GameMsg)
updateScene msg d =
    (case msg of
        MouseMove (x,_) -> { d | characterPosX = min x width}
        Tick t -> stepTime d t
        _ -> d
    , Cmd.none
    )

onMouseMove : Attribute GameMsg
onMouseMove =
  Html.Events.on "mousemove" (Decode.object2 (\x -> \y -> MouseMove (x, y))
      ("clientX" := Decode.float)
      ("clientY" := Decode.float))

render : GameData -> Html GameMsg
render d = div [onMouseMove] [toHtml (renderScene width height d)]

subscriptions : GameData -> Sub GameMsg
subscriptions d =
  Time.every (50 * Time.millisecond) Tick

main : Program Never
main = Html.App.program
 { init = initGameData
 , view = render
 , update = updateScene
 , subscriptions = subscriptions
 }
