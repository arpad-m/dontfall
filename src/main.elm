import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Html exposing (..)
import Html.App
import Color

b : Color.Color
b = Color.rgb 67 76 67

type GameData = A | B | C

renderScene : GameData -> Html msg
renderScene d = toHtml (collage 300 600 [filled b (rect 40 50)])

updateScene : msg -> GameData -> GameData
updateScene m d = A

main : Program Never
main = Html.App.beginnerProgram
 { model = B
 , view = renderScene
 , update = updateScene
 }