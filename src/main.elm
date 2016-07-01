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

import BaseStuff exposing (..)
import Render exposing (..)

speed = 100 / 1000

worldRandomize : Generator a -> GameData -> (a, GameData)
worldRandomize gen d =
    let
        (gena, nseed) = step gen d.worldrand
    in
        (gena, { d | worldrand = nseed})

oneIn : Int -> Generator Bool
oneIn i = map ((==) 0) (int 0 i)

maybeRandom : Generator Bool -> Generator a -> Generator (Maybe a)
maybeRandom bgen gen =
    map2 (\b -> \c -> if b then Just c else Nothing) bgen gen

maybeAddPlatform : Float -> GameData -> GameData
maybeAddPlatform pixeldiff d =
    let (possiblyAPosition, ndata) =
        worldRandomize (maybeRandom (oneIn 20) (float 0 d.flWidth)) d
    in
        case possiblyAPosition of
            Just pos -> { ndata | platforms = ndata.platforms ++ [(pos, d.flHeight)]}
            Nothing -> ndata

stepTime : GameData -> Time.Time -> GameData
stepTime d t =
    let
        pixeldiff = speed * Time.inMilliseconds (t - d.time)
    in
        let nd =
            maybeAddPlatform pixeldiff d
        in
            {nd | platforms = (List.map (\(x,y) -> (x, y - pixeldiff)) (nd.platforms)), time = t}

updateScene : GameMsg -> GameData -> (GameData, Cmd GameMsg)
updateScene msg d =

    (if d.paused then
        case msg of
            PauseToogle -> { d | paused = not d.paused }
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
