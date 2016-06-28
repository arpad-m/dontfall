import Text exposing (..)
import Html exposing (..)
import Html.App
import Element exposing (..)

import BaseTypes exposing (..)
import Render exposing (..)

updateScene : GameMsg -> GameData -> GameData
updateScene m d = A

render : GameData -> Html GameMsg
render d = div [] [toHtml (renderScene d)]

main : Program Never
main = Html.App.beginnerProgram
 { model = B
 , view = render
 , update = updateScene
 }