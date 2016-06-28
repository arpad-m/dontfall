module BaseTypes exposing (..)
import Time exposing (Time)

-- The actual types

type alias GameData =
    { characterPosX : Float
    , characterPosY : Float
    , time : Time
    }

type GameMsg = G | MouseMove (Float, Float) | Tick Time

-- Base functions

initGameData : (GameData, Cmd GameMsg)
initGameData =
    (
        { characterPosX = 0
        , characterPosY = 0
        , time = 0
        }
    , Cmd.none
    )
