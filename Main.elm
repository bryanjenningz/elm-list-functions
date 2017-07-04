module Main exposing (..)

import Html exposing (Html, text)


isEmpty_ : List a -> Bool
isEmpty_ list =
    case list of
        [] ->
            True

        _ ->
            False


isEmpty_2 : List a -> Bool
isEmpty_2 list =
    List.length list == 0


isEmpty_3 : List a -> Bool
isEmpty_3 list =
    list == []


isEmpty_4 : List a -> Bool
isEmpty_4 =
    (==) []


length_ : List a -> Int
length_ list =
    case list of
        [] ->
            0

        first :: rest ->
            1 + length_ rest


length_2 : List a -> Int
length_2 list =
    List.map (\_ -> 1) list |> List.sum


length_3 : List a -> Int
length_3 list =
    list |> List.map (always 1) |> List.sum


length_4 : List a -> Int
length_4 =
    List.map (always 1) >> List.sum


reverse_ : List a -> List a
reverse_ list =
    case list of
        [] ->
            []

        first :: rest ->
            reverse_ rest ++ [ first ]


reverse_2 : List a -> List a
reverse_2 list =
    List.foldl (::) [] list


reverse_3 : List a -> List a
reverse_3 =
    List.foldl (::) []


member_ : a -> List a -> Bool
member_ value list =
    case list of
        [] ->
            False

        first :: rest ->
            first == value || member_ value rest


member_2 : a -> List a -> Bool
member_2 value list =
    (List.filter ((==) value) list |> List.length) > 0


member_3 : a -> List a -> Bool
member_3 value list =
    List.any ((==) value) list


head_ : List a -> Maybe a
head_ list =
    case list of
        [] ->
            Nothing

        first :: _ ->
            Just first


tail_ : List a -> Maybe (List a)
tail_ list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just rest


filter_ : (a -> Bool) -> List a -> List a
filter_ test list =
    case list of
        [] ->
            []

        first :: rest ->
            if test first then
                first :: filter_ test rest
            else
                filter_ test rest


filter_2 : (a -> Bool) -> List a -> List a
filter_2 test list =
    List.foldr
        (\x xs ->
            if test x then
                x :: xs
            else
                xs
        )
        []
        list


take_ : Int -> List a -> List a
take_ amount list =
    if amount <= 0 then
        []
    else
        case list of
            [] ->
                []

            first :: rest ->
                first :: take_ (amount - 1) rest


take_2 : Int -> List a -> List a
take_2 amount list =
    List.map2 (,) (List.range 1 (List.length list)) list
        |> List.filter (Tuple.first >> (\i -> i < amount))
        |> List.map Tuple.second


take_3 : Int -> List a -> List a
take_3 amount list =
    list
        |> List.indexedMap
            (\i x ->
                if i < amount then
                    Just x
                else
                    Nothing
            )
        |> List.filterMap identity


drop_ : Int -> List a -> List a
drop_ amount list =
    if amount <= 0 then
        list
    else
        case list of
            [] ->
                []

            first :: rest ->
                drop_ (amount - 1) rest


singleton_ : a -> List a
singleton_ x =
    [ x ]


repeat_ : Int -> a -> List a
repeat_ amount x =
    if amount <= 0 then
        []
    else
        x :: repeat_ (amount - 1) x


range_ : Int -> Int -> List Int
range_ lo hi =
    if lo > hi then
        []
    else
        lo :: range_ (lo + 1) hi


append_ : List a -> List a -> List a
append_ xs ys =
    xs ++ ys


append_2 : List a -> List a -> List a
append_2 =
    (++)


concat_ : List (List a) -> List a
concat_ lists =
    case lists of
        [] ->
            []

        list :: otherLists ->
            list ++ concat_ otherLists


concat_2 : List (List a) -> List a
concat_2 lists =
    List.foldl (\lists list -> list ++ lists) [] lists


concat_3 : List (List a) -> List a
concat_3 lists =
    List.foldl (flip (++)) [] lists


concat_4 : List (List a) -> List a
concat_4 lists =
    List.foldr (++) [] lists


concat_5 : List (List a) -> List a
concat_5 =
    List.foldr (++) []


intersperse_ : a -> List a -> List a
intersperse_ betweenValue list =
    case list of
        [] ->
            []

        first :: [] ->
            [ first ]

        first :: rest ->
            first :: betweenValue :: intersperse_ betweenValue rest


intersperse_2 : a -> List a -> List a
intersperse_2 betweenValue list =
    List.concatMap (\x -> [ betweenValue, x ]) list
        |> List.drop 1


partition_ : (a -> Bool) -> List a -> ( List a, List a )
partition_ test list =
    ( List.filter test list, List.filter (not << test) list )


unzip_ : List ( a, b ) -> ( List a, List b )
unzip_ tuples =
    ( List.map Tuple.first tuples, List.map Tuple.second tuples )


map_ : (a -> b) -> List a -> List b
map_ f list =
    case list of
        [] ->
            []

        first :: rest ->
            f first :: map_ f rest


map_2 : (a -> b) -> List a -> List b
map_2 f list =
    List.foldl (\x xs -> xs ++ [ f x ]) [] list


map2_ : (a -> b -> c) -> List a -> List b -> List c
map2_ f list1 list2 =
    case list1 of
        [] ->
            []

        first1 :: rest1 ->
            case list2 of
                [] ->
                    []

                first2 :: rest2 ->
                    f first1 first2 :: map2_ f rest1 rest2


filterMap_ : (a -> Maybe b) -> List a -> List b
filterMap_ f list =
    case list of
        [] ->
            []

        first :: rest ->
            case f first of
                Just value ->
                    value :: filterMap_ f rest

                Nothing ->
                    filterMap_ f rest


concatMap_ : (a -> List b) -> List a -> List b
concatMap_ f list =
    List.concat <| List.map f list


concatMap_2 : (a -> List b) -> List a -> List b
concatMap_2 f =
    List.concat << List.map f


indexedMap_ : (Int -> a -> b) -> List a -> List b
indexedMap_ f list =
    List.map2
        f
        (List.range 0 (List.length list))
        list


foldr_ : (a -> b -> b) -> b -> List a -> b
foldr_ f init list =
    List.foldl f init (List.reverse list)


foldl_ : (a -> b -> b) -> b -> List a -> b
foldl_ f init list =
    case list of
        [] ->
            init

        first :: rest ->
            foldl_ f (f first init) rest


sum_ : List number -> number
sum_ list =
    case list of
        [] ->
            0

        first :: rest ->
            first + sum_ rest


sum_2 : List number -> number
sum_2 =
    List.foldl (+) 0


product_ : List number -> number
product_ =
    List.foldl (*) 1


maximum_ : List comparable -> Maybe comparable
maximum_ list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just <|
                List.foldl
                    (\x max ->
                        if x > max then
                            x
                        else
                            max
                    )
                    first
                    rest


minimum_ : List comparable -> Maybe comparable
minimum_ list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just <|
                List.foldl
                    (\x min ->
                        if x < min then
                            x
                        else
                            min
                    )
                    first
                    rest


all_ : (a -> Bool) -> List a -> Bool
all_ test list =
    (List.filter test list |> List.length) == List.length list


any_ : (a -> Bool) -> List a -> Bool
any_ test list =
    (List.filter test list |> List.length) > 0


scanl_ : (a -> b -> b) -> b -> List a -> List b
scanl_ f init list =
    List.foldl
        (\x ( ys, y ) ->
            ( f x y :: ys, f x y )
        )
        ( [ init ], init )
        list
        |> Tuple.first
        |> List.reverse


sort_ : List comparable -> List comparable
sort_ list =
    case list of
        [] ->
            []

        first :: [] ->
            [ first ]

        first :: rest ->
            let
                ( lows, highs ) =
                    List.partition (\x -> x < first) list
            in
                sort_ lows ++ [ first ] ++ sort_ highs


sortWith_ : (a -> a -> Order) -> List a -> List a
sortWith_ compare list =
    case list of
        [] ->
            []

        first :: [] ->
            [ first ]

        first :: rest ->
            List.foldl
                (\x ( lows, mids, highs ) ->
                    case compare x first of
                        LT ->
                            ( x :: lows, mids, highs )

                        EQ ->
                            ( lows, x :: mids, highs )

                        GT ->
                            ( lows, mids, x :: highs )
                )
                ( [], [ first ], [] )
                rest
                |> (\( a, b, c ) ->
                        sortWith_ compare a ++ b ++ sortWith_ compare c
                   )


main : Html a
main =
    text <| toString <| scanl_ (+) 0 (List.range 1 5)
