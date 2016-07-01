module Main exposing (..)

import Text exposing (..)
import Html exposing (..)
import Html.App
import Html.Events
import Time
import Element exposing (..)
import Json.Decode as Decode exposing ((:=))
import Random exposing (..)
import AnimationFrame
import Keyboard
import Char
import Murmur3
import Debug

import BaseStuff exposing (..)
import Render exposing (..)
import Platforms exposing (getWorldPlatforms)

speed = 100 / 1000
playerSpeed = 234 / 1000

addNewPlatforms : Float -> GameData -> GameData
addNewPlatforms pixeldiff d =
    { d | platforms = getWorldPlatforms d (d.gameWinY + d.flHeight) pixeldiff ++ d.platforms}

removeOldPlatforms : GameData -> GameData
removeOldPlatforms d = {
    d |
    platforms = List.filter (\(x, y) -> y - d.gameWinY >= -30) d.platforms
    }

updatePlayerY : Time.Time -> GameData -> GameData
updatePlayerY t d =
    let pixeldiff = if d.jumpPressed then playerSpeed * Time.inMilliseconds (t - d.time) else 0 in
        { d | characterPosY = pixeldiff + d.characterPosY }

stepTime : GameData -> Time.Time -> GameData
stepTime d t =
    let
        pixeldiff = speed * Time.inMilliseconds (t - d.time)
    in
        d
        |> addNewPlatforms pixeldiff
        |> updatePlayerY t
        |> \nd -> {nd | gameWinY = nd.gameWinY + pixeldiff, time = t}
        |> removeOldPlatforms

updateScene : GameMsg -> GameData -> (GameData, Cmd GameMsg)
updateScene msg d =

    (if d.paused then
        case msg of
            PauseToogle -> { d | paused = not d.paused }
            Tick t -> { d | time = t}
            _ -> d
    else
        case msg of
            MouseMove (x,_) -> { d | characterPosX = min x d.flWidth}
            Tick t -> stepTime d t
            PauseToogle -> { d | paused = not d.paused }
            JumpDown -> { d | jumpPressed = True }
            JumpUp -> { d | jumpPressed = False }
            _ -> d
    , Cmd.none
    )

onMouseMove : Attribute GameMsg
onMouseMove =
  Html.Events.on "mousemove" (Decode.object2 (\x -> \y -> MouseMove (x, y))
      ("offsetX" := Decode.float)
      ("offsetY" := Decode.float))

render : GameData -> Html GameMsg
render d = div [onMouseMove] [toHtml (renderScene d)]

subscriptions : GameData -> Sub GameMsg
subscriptions d =
    Sub.batch
        [ AnimationFrame.times Tick
        , Keyboard.downs (\c -> if Char.fromCode c == 'P' then PauseToogle else NothingHappened)
        , Keyboard.downs (\c -> if Char.fromCode c == ' ' then JumpDown else NothingHappened)
        , Keyboard.ups (\c -> if Char.fromCode c == ' ' then JumpUp else NothingHappened)
        ]

main : Program InitFlags
main = Html.App.programWithFlags
 { init = initGameData
 , view = render
 , update = updateScene
 , subscriptions = subscriptions
 }
