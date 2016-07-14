module Util exposing
    (extremestWith
    , biggestInList
    , smallestInList
    )

extremestWith : (a -> a -> a) -> List a -> Maybe a
extremestWith ext l =
    case (List.head l, List.tail l) of
        (Nothing, Nothing) -> Nothing
        (Just hd, Just tl) -> Just (case extremestWith ext tl of
            Nothing -> hd
            Just d -> ext d hd)
        _ -> Nothing -- error, may not happen

biggestInList : List comparable -> Maybe comparable
biggestInList = extremestWith (\v w -> if v > w then v else w)

smallestInList : List comparable -> Maybe comparable
smallestInList = extremestWith (\v w -> if v < w then v else w)
