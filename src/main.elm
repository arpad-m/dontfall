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
import Debug

import BaseStuff exposing (..)
import Render exposing (..)
import Platforms exposing (genPlatforms, platformDistance)

speed { gameWinY } = (min 3 <| max 1 <| gameWinY / 5000) * 100 / 1000

addNewPlatforms : Float -> GameData -> GameData
addNewPlatforms pixeldiff d =
  let
    (newPlatforms, nextSeed) = step (genPlatforms d.flWidth (d.gameWinY + 2 * d.flHeight) pixeldiff) d.seed
  in  
    { d | platforms = newPlatforms ++ d.platforms, seed = nextSeed}

-- This constant should be lower than the maximum value of the jump parabole.
platformMaxDistance : Float
platformMaxDistance = 8 * platformDistance

getClosestAbove : List (Float, Float) -> Float -> Maybe Float
getClosestAbove l py = l
    |> List.filterMap (\(x, y) -> if y > py then Just y else Nothing)
    |> List.minimum

getPlatformsWithGapsAbove : List (Float, Float) -> List (Float, Int)
getPlatformsWithGapsAbove platforms = List.filterMap (\(_, ply) ->
    Maybe.map (\yb -> (ply, floor <| (yb - ply) / platformMaxDistance))
        (getClosestAbove platforms ply)) platforms

-- Fill the gaps between the platforms
-- so that the player has the possibility to survive
fillInPlatforms : GameData -> GameData
fillInPlatforms d =
    let
        fillerplatforms = (getPlatformsWithGapsAbove d.platforms
        |> List.concatMap (\(ply, cnt) ->
            if cnt == 0 then
                []
            else
                List.map (\n -> (30, ply + (toFloat n) * platformMaxDistance)) [1 .. cnt]
        ))
    in
        { d | platforms = (d.platforms ++ fillerplatforms) }

removeOldPlatforms : GameData -> GameData
removeOldPlatforms d = {
    d |
    platforms = List.filter (\(x, y) -> y - d.gameWinY >= -30) d.platforms
    }

intsOverlap : Float -> Float -> Float -> Float -> Bool
intsOverlap i j k l = (max i k) <= (min j l)

smd : Float -> Float -> Bool
smd a b = a <= b && a > b - 12

playerXcollidesWithPlatform : Float -> (Float, Float) -> Bool
playerXcollidesWithPlatform characterPosX (plx, ply) =
    intsOverlap (plx - (platformWidth / 2)) (plx + (platformWidth / 2)) (characterPosX - (playerWidth / 2)) (characterPosX + (playerWidth / 2))

playerCollidesPlatformInFall : Float -> Float -> Float -> (Float, Float) -> Bool
playerCollidesPlatformInFall characterPosX characterPosYmin characterPosYmax (plx, ply) =
    playerXcollidesWithPlatform characterPosX (plx, ply) &&
        (characterPosYmin - (playerHeight / 2)) <= ply &&
        (characterPosYmax - (playerHeight / 2)) >= ply

-- Returns Just f if a player collides with a platform,
-- with the player being at y coord f
-- Returns Noting if there is no collision
playerCollidesDuringFall : Float -> GameData -> Maybe Float
playerCollidesDuringFall newCharacterPosY { characterPosX, characterPosY, platforms } =
    let (minPosY, maxPosY) =
        (min characterPosY newCharacterPosY, max characterPosY newCharacterPosY)
    in
        List.filter (\pl -> playerCollidesPlatformInFall characterPosX minPosY maxPosY pl) platforms
        |> List.map (\(plx, ply) -> ply)
        -- If we fall, we need the highest platform we collide with,
        -- If we jump, we need the lowest one.
        |> (if newCharacterPosY < characterPosY then List.maximum else List.minimum)
        |> Maybe.map (\p -> p + (playerHeight / 2))

-- This is the usual jump parabole: raising at the start, then falling later on.
calcJumpCurve : Float -> Float
calcJumpCurve t = (250000 - (t - 500)^2) / 1000

jumpTippingPoint : Float
jumpTippingPoint = 460

tippingPointY : Float
tippingPointY = calcJumpCurve jumpTippingPoint

jumpTippingPointTime : Time.Time
jumpTippingPointTime = jumpTippingPoint * Time.millisecond

putAtTippingPoint : Time.Time -> GameData -> GameData
putAtTippingPoint t d = { d | jumpPressedTimeY = Just (t - jumpTippingPointTime, d.characterPosY - tippingPointY) }

updatePlayerY : Time.Time -> GameData -> GameData
updatePlayerY t d =
    case d.jumpPressedTimeY of
        Nothing -> putAtTippingPoint t d
        Just (ljt, ljy) -> let
                pixeldiff = (calcJumpCurve <| Time.inMilliseconds (t - ljt))
            in
                case playerCollidesDuringFall (pixeldiff + ljy) d of
                    Nothing -> { d | characterPosY = pixeldiff + ljy }
                    Just y -> d
                        |> \nd -> { nd | characterPosY = y }
                        |> \nd -> (if d.jumpPressed then
                                {nd | jumpPressedTimeY = Just (t, nd.characterPosY), characterPosY = nd.characterPosY + 1}
                            else putAtTippingPoint t nd)

stepTime : GameData -> Time.Time -> GameData
stepTime d t =
    let
        pixeldiff = max
            (speed d * Time.inMilliseconds (t - d.time))
            (d.characterPosY - (d.gameWinY + d.flHeight))
    in
        d
        |> addNewPlatforms pixeldiff
        |> fillInPlatforms
        |> updatePlayerY t
        |> \nd -> {nd | gameWinY = nd.gameWinY + pixeldiff, time = t}
        |> removeOldPlatforms
        |> \nd -> if nd.characterPosY < nd.gameWinY then { nd | state = GameOver } else nd

updateScene : GameMsg -> GameData -> (GameData, Cmd GameMsg)
updateScene msg d =

    (case d.state of
        GameOver -> case msg of
            PauseToogle -> let r = resetGameData d in { r | state = Running, time = d.time }
            Tick t -> { d | time = t}
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
