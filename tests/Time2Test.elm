module Time2Test exposing (..)

import Expect
import Fuzz
import Test exposing (Test)
import Time
import Time2


{-| Extract several eras from America/New\_York for testing
-}
newYorkZone : Time2.Zone
newYorkZone =
    Time2.customZone ""
        [ { start = 27976740, offset = -240 } -- 2023-03-12T03:00:00-04:00
        , { start = 27795240, offset = -300 } -- 2022-11-06T01:00:00-05:00
        , { start = 27452580, offset = -240 } -- 2022-03-13T03:00:00-04:00
        , { start = 27271080, offset = -300 } -- 2021-11-07T01:00:00-05:00
        ]
        -300


toPartsAndFromParts : Test
toPartsAndFromParts =
    Test.fuzz (Fuzz.intRange (27271080 * 60) (27976740 * 60))
        "toPartsAndFromParts"
        (\second ->
            let
                parts =
                    second
                        * 1000
                        |> Time.millisToPosix
                        |> Time2.toParts newYorkZone
            in
            parts
                |> Time2.fromParts newYorkZone
                |> Time2.toParts newYorkZone
                |> Expect.equal parts
        )
