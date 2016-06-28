module BaseStuff exposing (..)
import Time exposing (Time)
import Random exposing (Seed, initialSeed)

-- The actual types

type alias GameData =
    { characterPosX : Float
    , characterPosY : Float
    , time : Time
    , platforms : List (Float, Float)
    , worldrand : Seed
    }

type GameMsg = G | MouseMove (Float, Float) | Tick Time

-- Base functions

initGameData : (GameData, Cmd GameMsg)
initGameData =
    (
        { characterPosX = 0
        , characterPosY = 0
        , time = 0
        , platforms = []
        , worldrand = initialSeed 84763
        }
    , Cmd.none
    )


-- Random helper function

generateRandList : Int -> (Seed -> (List o, Seed)) -> Seed -> (List o, Seed)
generateRandList len rfn seed =
    case len of
        0 -> ([], seed)
        len -> let (rlist, rseed) =
                (generateRandList (len - 1) rfn seed)
            in
                let (c, nseed) =
                    rfn rseed
                in
                    (c ++ rlist, nseed)