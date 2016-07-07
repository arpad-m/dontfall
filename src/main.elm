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

intsOverlap : Float -> Float -> Float -> Float -> Bool
intsOverlap i j k l = (max i k) <= (min j l)

smd : Float -> Float -> Bool
smd a b = a <= b && a > b - 12

playerStandsOnPlatform : GameData -> Bool
playerStandsOnPlatform { platforms, characterPosX, characterPosY } =
    List.any (\(plx, ply) -> (smd ply (characterPosY - (playerHeight / 2))) &&
        intsOverlap (plx - (platformWidth / 2)) (plx + (platformWidth / 2)) (characterPosX - (playerWidth / 2)) (characterPosX + (playerWidth / 2))) platforms

-- This is the usual jump parabole: raising at the start, then falling later on.
-- Its not 0 at 0, but a small positive value,
-- because otherwise we wouldn't get "off" a platform,
-- and would be trapped on it forever. The small advancement
-- gives us a small boost to get off the platform.
calcJumpCurve : Float -> Float
calcJumpCurve t = (250000 - (t - 500 + 30)^2) / 1000

updatePlayerY : Time.Time -> GameData -> GameData
updatePlayerY t d =
    if (not d.jumpPressed) && playerStandsOnPlatform d then d else
    let startjump = (d.jumpPressed && playerStandsOnPlatform d) in
        d
        |> \nd -> (if startjump then {nd | jumpPressedTimeY = Just (t, nd.characterPosY)} else nd)
        |> \nd -> case nd.jumpPressedTimeY of
            Nothing -> {nd | jumpPressedTimeY = Just (t, nd.characterPosY)}
            Just (ljt, ljy) -> let pixeldiff = (calcJumpCurve <| Time.inMilliseconds (t - ljt)) in
                { nd | characterPosY = pixeldiff + ljy }

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
        |> \nd -> if nd.characterPosY < nd.gameWinY then { nd | state = GameOver } else nd

updateScene : GameMsg -> GameData -> (GameData, Cmd GameMsg)
updateScene msg d =

    (case d.state of
        GameOver -> case msg of
            PauseToogle -> let r = resetGameData d in { r | state = Running, time = d.time }
            _ -> d
        Paused -> case msg of
            PauseToogle -> { d | state = Running }
            Tick t -> { d
                | time = t
                , jumpPressedTimeY = Maybe.map (\(ljt,ljy) -> (ljt + t - d.time, ljy)) d.jumpPressedTimeY}
            _ -> d
        Running -> case msg of
            MouseMove (x,_) -> { d | characterPosX = min x d.flWidth}
            Tick t -> stepTime d t
            PauseToogle -> { d | state = Paused }
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
