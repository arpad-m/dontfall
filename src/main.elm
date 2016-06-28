import Text exposing (..)
import Html exposing (..)
import Html.App
import Html.Events
import Element exposing (..)
import Json.Decode as Decode exposing ((:=))

import BaseTypes exposing (..)
import Render exposing (..)

height = 600
width = 300

updateScene : GameMsg -> GameData -> GameData
updateScene msg d =
    case msg of
        MouseMove (x,_) -> { d | characterPosX = x}
        _ -> d

onMouseMove : Attribute GameMsg
onMouseMove =
  Html.Events.on "mousemove" (Decode.object2 (\x -> \y -> MouseMove (x, y))
      ("clientX" := Decode.float)
      ("clientY" := Decode.float))

render : GameData -> Html GameMsg
render d = div [onMouseMove] [toHtml (renderScene width height d)]

main : Program Never
main = Html.App.beginnerProgram
 { model = initGameData
 , view = render
 , update = updateScene
 }
