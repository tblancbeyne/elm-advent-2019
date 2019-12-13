module Aoc2 exposing (..)

import Array exposing (Array, fromList)


type alias Program =
    Array Int


interpret : Int -> Program -> Maybe Program
interpret idx program =
    case Array.get idx program of
        Nothing ->
            Nothing

        Just op ->
            operate op idx program


operate : Int -> Int -> Program -> Maybe Program
operate op idx program =
    case op of
        1 ->
            Maybe.map3
                (add program)
                (getValue program (idx + 1))
                (getValue program (idx + 2))
                (Array.get (idx + 3) program)
                |> Maybe.andThen (interpret (idx + 4))

        2 ->
            Maybe.map3
                (mult program)
                (getValue program (idx + 1))
                (getValue program (idx + 2))
                (Array.get (idx + 3) program)
                |> Maybe.andThen (interpret (idx + 4))

        99 ->
            Just program

        _ ->
            Nothing


getValue : Program -> Int -> Maybe Int
getValue program idx =
    Array.get idx program
        |> Maybe.andThen (\val -> Array.get val program)


add : Program -> Int -> Int -> Int -> Program
add program a b result =
    Array.set result (a + b) program


mult : Program -> Int -> Int -> Int -> Program
mult program a b result =
    Array.set result (a * b) program


findInput : ( Int, Int ) -> Int -> Program -> ( Int, Int )
findInput ( a, b ) expected program =
    let
        tmp =
            Array.set 1 a program

        up =
            Array.set 2 b tmp

        val =
            interpret 0 up
                |> Maybe.andThen (Array.get 0)
    in
    case val of
        Just v ->
            if v < expected then
                findInput ( a + 1, b ) expected program

            else if v > expected then
                findInput ( a - 1, b + 1 ) expected program

            else
                ( a, b )

        Nothing ->
            ( 0, 0 )


test : Array Int
test =
    fromList
        [ 1
        , 9
        , 10
        , 3
        , 2
        , 3
        , 11
        , 0
        , 99
        , 30
        , 40
        , 50
        ]


input : Array Int
input =
    fromList
        [ 1
        , 0
        , 0
        , 3
        , 1
        , 1
        , 2
        , 3
        , 1
        , 3
        , 4
        , 3
        , 1
        , 5
        , 0
        , 3
        , 2
        , 1
        , 9
        , 19
        , 1
        , 19
        , 5
        , 23
        , 2
        , 6
        , 23
        , 27
        , 1
        , 6
        , 27
        , 31
        , 2
        , 31
        , 9
        , 35
        , 1
        , 35
        , 6
        , 39
        , 1
        , 10
        , 39
        , 43
        , 2
        , 9
        , 43
        , 47
        , 1
        , 5
        , 47
        , 51
        , 2
        , 51
        , 6
        , 55
        , 1
        , 5
        , 55
        , 59
        , 2
        , 13
        , 59
        , 63
        , 1
        , 63
        , 5
        , 67
        , 2
        , 67
        , 13
        , 71
        , 1
        , 71
        , 9
        , 75
        , 1
        , 75
        , 6
        , 79
        , 2
        , 79
        , 6
        , 83
        , 1
        , 83
        , 5
        , 87
        , 2
        , 87
        , 9
        , 91
        , 2
        , 9
        , 91
        , 95
        , 1
        , 5
        , 95
        , 99
        , 2
        , 99
        , 13
        , 103
        , 1
        , 103
        , 5
        , 107
        , 1
        , 2
        , 107
        , 111
        , 1
        , 111
        , 5
        , 0
        , 99
        , 2
        , 14
        , 0
        , 0
        ]
