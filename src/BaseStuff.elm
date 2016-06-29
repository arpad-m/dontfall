module BaseStuff exposing (..)
import Time exposing (Time)
import Random exposing (Seed, initialSeed)
import Collage exposing (Form)
import Background exposing (background)

-- The actual types

type alias InitFlags =
    { width : Int
    , height : Int
    , seed : Int
    }

type alias GameData =
    { width : Int
    , height : Int
    , flWidth : Float
    , flHeight : Float
    , characterPosX : Float
    , characterPosY : Float
    , time : Time
    , platforms : List (Float, Float)
    , worldrand : Seed
    , background : Form
    }

type GameMsg = G | MouseMove (Float, Float) | Tick Time

-- Base functions

initGameData : InitFlags -> (GameData, Cmd GameMsg)
initGameData initFl =
    (
        { width = initFl.width
        , height = initFl.height
        , flWidth = toFloat initFl.width
        , flHeight = toFloat initFl.height
        , characterPosX = 0
        , characterPosY = 0
        , time = 0
        , platforms = []
        , worldrand = (initialSeed initFl.seed)
        , background = background (toFloat initFl.width) (toFloat initFl.height)
        }
    , Cmd.none
    )
