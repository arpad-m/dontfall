module Render exposing
    (platformWidth
    , playerWidth
    , playerHeight
    , renderScene
    )

import Element exposing (..)
import Collage exposing (..)
import String
import Text
import Color

import BaseStuff exposing (..)
import Random exposing (..)

platformColor : Color.Color
platformColor = Color.rgb 100 19 67

plFill : Shape -> Form
plFill = (filled platformColor)

platformWidth : Float
platformWidth = 76

platformModel : Form
platformModel = group [plFill (rect 60 16), move (-30, 0) (plFill (circle 8)), move (30, 0) (plFill (circle 8))]

playerColor : Color.Color
playerColor = Color.rgb 0 128 0

playerWidth : Float
playerWidth = 40

playerHeight : Float
playerHeight = 50

playerModel : Form
playerModel = filled playerColor (rect playerWidth playerHeight)

renderPlatform : GameData -> (Float, Float) -> Form
renderPlatform { flWidth, flHeight, gameWinY } (x, y) =
    move (x - flWidth / 2, y - flHeight / 2 - gameWinY) platformModel

pausedText : Text.Text
pausedText = Text.color Color.white (Text.fromString "Game paused. Press [P] to resume.")

gameOverString : GameData -> String
gameOverString { gameWinY } = String.concat ["Game over. Reached height: ", toString (round gameWinY)]

gameOverText : GameData -> Text.Text
gameOverText d = Text.color Color.white (Text.fromString (gameOverString d))

newGameStartText : Text.Text
newGameStartText = Text.color Color.white (Text.fromString "Press [P] to start new game.")

renderScene : GameData -> Element
renderScene d = collage d.width d.height
    (
        [ d.background ]
        ++ List.map (renderPlatform d) d.platforms
        ++ [ move (d.characterPosX - d.flWidth / 2, d.characterPosY - d.flHeight/ 2 - d.gameWinY) playerModel ]
        ++ if d.state == Paused then [ text pausedText ] else []
        ++ if d.state == GameOver then [ text (gameOverText d),
            move (0, -17) <| text newGameStartText ] else []
    )
