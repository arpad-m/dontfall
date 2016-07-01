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

speed = 100 / 1000

oneIn : Int -> Generator Bool
oneIn i = map ((==) 0) (int 0 i)

maybeRandom : Generator Bool -> Generator a -> Generator (Maybe a)
maybeRandom bgen gen =
    map2 (\b -> \c -> if b then Just c else Nothing) bgen gen

seedAtPoint : Int -> Int -> Seed
seedAtPoint i j = initialSeed <| Murmur3.hashString i (toString j)

worldRandomizeAtPoint : Int -> Int -> Generator a -> a
worldRandomizeAtPoint worldSeed p gen = let (s, _) = step gen (seedAtPoint worldSeed p) in s

rangeTo : Int -> List Int
rangeTo a = List.indexedMap (\i -> \v -> i) (List.repeat a ())

platformDistance : Float
platformDistance = 30

pldf : Float -> Int
pldf f = floor (f / platformDistance)

ipldf : Int -> Float
ipldf f = (toFloat f) * platformDistance

getWorldPlatforms { worldSeed, flWidth } yOffs pixeldiff =
    let (oldYInt, newYInt) = (pldf yOffs, pldf (yOffs + pixeldiff)) in
        List.filterMap
            (\n -> worldRandomizeAtPoint worldSeed (n + oldYInt)
                (maybeRandom (oneIn 2) (Random.map (\x -> (x, ipldf (n + oldYInt))) (float 0 flWidth)))
            )
            (rangeTo (newYInt - oldYInt))

addNewPlatforms : Float -> GameData -> GameData
addNewPlatforms pixeldiff d =
    { d | platforms = getWorldPlatforms d (d.gameWinY + d.flHeight) pixeldiff ++ d.platforms}

removeOldPlatforms : GameData -> GameData
removeOldPlatforms d = {
    d |
    platforms = List.filter (\(x, y) -> y - d.gameWinY >= -30) d.platforms
    }

stepTime : GameData -> Time.Time -> GameData
stepTime d t =
    let
        pixeldiff = speed * Time.inMilliseconds (t - d.time)
    in
        d
        |> addNewPlatforms pixeldiff
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
        ]

main : Program InitFlags
main = Html.App.programWithFlags
 { init = initGameData
 , view = render
 , update = updateScene
 , subscriptions = subscriptions
 }
