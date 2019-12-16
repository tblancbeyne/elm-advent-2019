module Aoc4 exposing (..)

import Array exposing (Array, fromList)


type alias Password =
    Int


check : Int -> Int -> Int
check idx cnt =
    let
        a =
            idx // 100000

        b =
            (idx - a * 100000) // 10000

        c =
            (idx - (a * 10 + b) * 10000) // 1000

        d =
            (idx - (((a * 10 + b) * 10) + c) * 1000) // 100

        e =
            (idx - ((((a * 10 + b) * 10) + c) * 10 + d) * 100) // 10

        f =
            idx - (((((a * 10 + b) * 10) + c) * 10 + d) * 10 + e) * 10
    in
    if idx <= 675810 then
        if
            ((a == b && b /= c) || (a /= b && b == c && c /= d) || (b /= c && c == d && d /= e) || (c /= d && d == e && e /= f) || (d /= e && e == f))
                && (a <= b && b <= c && c <= d && d <= e && e <= f)
        then
            check (idx + 1) (cnt + 1)

        else
            check (idx + 1) cnt

    else
        cnt


range =
    ( 134792, 675810 )
