module BaseTypes exposing (..)

-- The actual types

type alias GameData =
    { characterPosX : Float
    , characterPosY : Float
    }

type GameMsg = G | MouseMove (Float, Float) | S

-- Base functions

initGameData : GameData
initGameData =
    { characterPosX = 0
    , characterPosY = 0
    }
