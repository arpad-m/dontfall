module BaseStuff exposing (..)
import Time exposing (Time)
import Random exposing (Seed, initialSeed)
import Collage exposing (Form)
import Background exposing (background)

-- The actual types

type alias GameData =
    { characterPosX : Float
    , characterPosY : Float
    , time : Time
    , platforms : List (Float, Float)
    , worldrand : Seed
    , background : Form
    }

type GameMsg = G | MouseMove (Float, Float) | Tick Time

-- Base functions

initGameData : Float -> Float -> (GameData, Cmd GameMsg)
initGameData width height =
    (
        { characterPosX = 0
        , characterPosY = 0
        , time = 0
        , platforms = []
        , worldrand = initialSeed 84763
        , background = background width height
        }
    , Cmd.none
    )
