module Platforms exposing (..)

import Random exposing (..)
import Murmur3

oneIn : Int -> Generator Bool
oneIn i = map ((==) 0) (int 0 i)

maybeRandom : Generator Bool -> Generator a -> Generator (Maybe a)
maybeRandom bgen gen =
    map2 (\b -> \c -> if b then Just c else Nothing) bgen gen

seedAtPoint : Int -> Int -> Seed
seedAtPoint i j = initialSeed <| Murmur3.hashString i (toString j)

worldRandomizeAtPoint : Int -> Int -> Generator a -> a
worldRandomizeAtPoint worldSeed p gen = let (s, _) = step gen (seedAtPoint worldSeed p) in s

platformDistance : Float
platformDistance = 30

pldf : Float -> Int
pldf f = floor (f / platformDistance)

ipldf : Int -> Float
ipldf f = (toFloat f) * platformDistance

getWorldPlatforms { worldSeed, flWidth } yOffs pixeldiff =
    let (oldYInt, newYInt) = (pldf yOffs, pldf (yOffs + pixeldiff)) in
        List.filterMap
            (\n -> worldRandomizeAtPoint worldSeed n
                (maybeRandom (oneIn 2) (Random.map (\x -> (x, ipldf n)) (float 0 flWidth)))
            )
            [oldYInt .. newYInt - 1]

