module Platforms exposing (..)

import Random exposing (..)

oneIn : Int -> Generator Bool
oneIn i = map ((==) 0) (int 0 i)

maybeRandom : Generator Bool -> Generator a -> Generator (Maybe a)
maybeRandom bgen gen =
    map2 (\b -> \c -> if b then Just c else Nothing) bgen gen

platformDistance : Float
platformDistance = 30

pldf : Float -> Int
pldf f = floor (f / platformDistance)

ipldf : Int -> Float
ipldf f = (toFloat f) * platformDistance

genPlatforms : Float -> Float -> Float -> Generator (List (Float, Float))
genPlatforms flWidth yOffs pixeldiff =
    let (oldYInt, newYInt) = (pldf yOffs, pldf (yOffs + pixeldiff)) in
        Random.map (    
        List.filterMap
            (\(n, mx) -> Maybe.map (\x -> (x, ipldf n)) mx)
            ) <| (Random.map (List.map2 (,) [oldYInt .. newYInt - 1]) (Random.list (newYInt - oldYInt) (maybeRandom (oneIn 2) (float 0 flWidth))))

