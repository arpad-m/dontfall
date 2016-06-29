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

import BaseStuff exposing (..)
import Render exposing (..)

height = 600
width = 300

speed = 100 / 1000

worldRandomize : Generator a -> GameData -> (a, GameData)
worldRandomize gen d =
    let
        (gena, nseed) = step gen d.worldrand
    in
        (gena, { d | worldrand = nseed})

oneIn : Int -> Generator Bool
oneIn i = map ((==) 0) (int 0 i)

maybeRandom : Generator a -> Generator (Maybe a)
maybeRandom gen =
    map2 (\b -> \c -> if b then Just c else Nothing) (oneIn 20) gen

maybeAddPlatform : Float -> GameData -> GameData
maybeAddPlatform pixeldiff d =
    let (possiblyAPosition, ndata) =
        worldRandomize (maybeRandom (float (-d.flWidth/2) (d.flWidth / 2))) d
    in
        case possiblyAPosition of
            Just pos -> { ndata | platforms = ndata.platforms ++ [(pos, height)]}
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
    (case msg of
        MouseMove (x,_) -> { d | characterPosX = min x d.flWidth}
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
render d = div [onMouseMove] [toHtml (renderScene d)]

subscriptions : GameData -> Sub GameMsg
subscriptions d =
  AnimationFrame.times Tick

main : Program Never
main = Html.App.program
 { init = initGameData width height
 , view = render
 , update = updateScene
 , subscriptions = subscriptions
 }
