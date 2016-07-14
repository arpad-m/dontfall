module BaseStuff exposing (..)
import Time exposing (Time)
import Random exposing (Seed, initialSeed)
import Collage exposing (Form)

import Background exposing (background)
import Platforms exposing (genPlatforms)

-- The actual types

type alias InitFlags =
    { width : Int
    , height : Int
    , seed : Int
    }

type GameState = Running
    | Paused
    | GameOver

type alias GameData =
    { width : Int
    , height : Int
    , flWidth : Float
    , flHeight : Float
    , state : GameState
    , jumpPressed : Bool
    , jumpPressedTimeY : Maybe (Time, Float)
    , gameWinY : Float
    , characterPosX : Float
    , characterPosY : Float
    , time : Time
    , platforms : List (Float, Float)
    , seed : Seed
    , background : Form
    }

type GameMsg = NothingHappened
    | MouseMove (Float, Float)
    | Tick Time
    | PauseToogle
    | JumpDown
    | JumpUp

-- Base functions

initGameData : InitFlags -> (GameData, Cmd GameMsg)
initGameData { width, height, seed } = (initGameData' width height (initialSeed seed), Cmd.none)

initGameData' width height seed =
  let
    (platforms, nextSeed) = Random.step (genPlatforms (toFloat width) 0 (toFloat height)) seed
  in  
        { width = width
        , height = height
        , flWidth = toFloat width
        , flHeight = toFloat height
        , state = Paused
        , jumpPressed = False
        , jumpPressedTimeY = Nothing
        , gameWinY = 0
        , characterPosX = 0
        , characterPosY = 300
        , time = 0
        , platforms = platforms
        , seed = nextSeed
        , background = background (toFloat width) (toFloat height)
        }

resetGameData : GameData -> GameData
resetGameData { width, height, seed } =
        initGameData' width height seed
