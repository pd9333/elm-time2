module Time2 exposing
    ( Zone
    , customZone
    , decoderOfZone
    , encodeZone
    , epoch
    , utc
    )

import Json.Decode
import Json.Encode
import Time


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


{-| A zone is a list of eras sorted by descending order of the starting point of each era.
-}
type Zone
    = Zone
        { name : String

        -- To make impossible state impossible, the last era is extracted and put in a dedicated field so we should
        -- always have that era.
        , eras : List Era
        , offsetOfEarliestEra : Int
        }


customZone : String -> List { start : Int, offset : Int } -> Int -> Zone
customZone name eras offsetOfEarliestEra =
    Zone
        { name = name
        , eras = eras
        , offsetOfEarliestEra = offsetOfEarliestEra
        }


{-| Both `start` and `offset` of an era are minutes from `epoch`.
-}
type alias Era =
    { start : Int, offset : Int }


encodeEra : Era -> Json.Encode.Value
encodeEra { start, offset } =
    Json.Encode.object
        [ ( "s", Json.Encode.int start )
        , ( "o", Json.Encode.int offset )
        ]


decoderOfEra : Json.Decode.Decoder Era
decoderOfEra =
    Json.Decode.map2 Era
        (Json.Decode.field "s" Json.Decode.int)
        (Json.Decode.field "o" Json.Decode.int)


{-|

    import Json.Decode

    zone : Zone
    zone =
        customZone "Zone/Name"
            [ { start = 20, offset = 60 }
            , { start = 10, offset = -120 }
            ]
            30

    zone
        |> encodeZone
        |> Json.Decode.decodeValue decoderOfZone
    --> Ok zone

-}
decoderOfZone : Json.Decode.Decoder Zone
decoderOfZone =
    Json.Decode.map3 customZone
        (Json.Decode.field "n" Json.Decode.string)
        (Json.Decode.field "e" (Json.Decode.list decoderOfEra))
        (Json.Decode.field "o" Json.Decode.int)


encodeZone : Zone -> Json.Encode.Value
encodeZone (Zone { name, eras, offsetOfEarliestEra }) =
    Json.Encode.object
        [ ( "n", Json.Encode.string name )
        , ( "e", Json.Encode.list encodeEra eras )
        , ( "o", Json.Encode.int offsetOfEarliestEra )
        ]


utc : Zone
utc =
    customZone "UTC" [] 0
