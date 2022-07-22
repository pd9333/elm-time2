module Time2 exposing
    ( epoch
    , Parts, toParts, fromParts, withYear, withMonth, withDay, withHour, withMinute, withSecond, withMillis
    , Zone, customZone, toStandardZone, utc, encodeZone, decoderOfZone
    )

{-| This package allows us to transfer time zone through the wire easily and tries to observe daylight saving time.


# Time

@docs epoch


## Parts

@docs Parts, toParts, fromParts, withYear, withMonth, withDay, withHour, withMinute, withSecond, withMillis


# Time Zones

@docs Zone, customZone, toStandardZone, utc, encodeZone, decoderOfZone

-}

import Json.Decode
import Json.Encode
import Time


{-| _Unix epoch_
-}
epoch : Time.Posix
epoch =
    Time.millisToPosix 0


{-| A zone is a list of eras sorted by descending order of the starting point of each era.

Note that we don't expose the variants because we don't want to bump major version of the package too often. Use
[`customZone`](#customZone) if we want to create a zone manually. Or create a decoder to decode the value encoded by
[`encodeZone`](#encodeZone) if we want to get the raw data, we'll try to keep [`encodeZone`](#encodeZone) backward
compatible.

-}
type Zone
    = Zone
        { name : String

        -- To make impossible state impossible, the earliest era is extracted and put in a dedicated field so we should
        -- always have that era.
        , eras : List Era
        , offsetOfEarliestEra : Int
        }


{-| Create a zone with `name`, `eras` and `offsetOfEarliestEra`.
-}
customZone : String -> List { start : Int, offset : Int } -> Int -> Zone
customZone name eras offsetOfEarliestEra =
    Zone
        { name = name
        , eras = eras
        , offsetOfEarliestEra = offsetOfEarliestEra
        }


{-| This function converts [`Time2.Zone`](#Zone) to [`Time.Zone`][standardZone].

[standardZone]: /packages/elm/time/latest/Time#Zone

    import Time

    start : Int
    start =
        61

    zone : Time.Zone
    zone =
        customZone "" [ { start = start, offset = 120 } ] 0
            |> toStandardZone

    Time.millisToPosix (start * 60 * 1000)
        |> Time.toHour zone
    --> 3 -- This will be 1 once https://github.com/elm/time/issues/7 is fixed, we then need to fix our implementation

-}
toStandardZone : Zone -> Time.Zone
toStandardZone (Zone { eras, offsetOfEarliestEra }) =
    eras
        |> List.map (\{ start, offset } -> Era (start - 1) offset)
        |> Time.customZone offsetOfEarliestEra


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


{-| Encode [`Zone`](#Zone) so we can send it through wire.

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
encodeZone : Zone -> Json.Encode.Value
encodeZone (Zone { name, eras, offsetOfEarliestEra }) =
    Json.Encode.object
        [ ( "n", Json.Encode.string name )
        , ( "e", Json.Encode.list encodeEra eras )
        , ( "o", Json.Encode.int offsetOfEarliestEra )
        ]


{-| Decode `Json.Encode.Value` back to [`Zone`](#Zone).
-}
decoderOfZone : Json.Decode.Decoder Zone
decoderOfZone =
    Json.Decode.map3 customZone
        (Json.Decode.field "n" Json.Decode.string)
        (Json.Decode.field "e" (Json.Decode.list decoderOfEra))
        (Json.Decode.field "o" Json.Decode.int)


{-| utc
-}
utc : Zone
utc =
    customZone "UTC" [] 0



-- Parts


{-| Keep track of each part of time. Use [`toParts`](#toParts) to create parts from posix time.

    import Iso8601
    import Time

    epoch
        |> toParts utc
        |> withYear 2022
        |> withMonth Time.Jul
        |> withDay 22
        |> withHour 14
        |> withMinute 32
        |> withSecond 17
        |> withMillis 678
        |> fromParts utc
        |> Ok
    --> Iso8601.toTime "2022-07-22T14:32:17.678Z"

-}
type Parts
    = Parts
        { year : Int
        , month : Time.Month
        , day : Int
        , hour : Int
        , minute : Int
        , second : Int
        , millis : Int
        }


{-| Convert posix time to [`Parts`](#Parts).
-}
toParts : Zone -> Time.Posix -> Parts
toParts zone time =
    let
        standardZone =
            toStandardZone zone
    in
    Parts
        { year = Time.toYear standardZone time
        , month = Time.toMonth standardZone time
        , day = Time.toDay standardZone time
        , hour = Time.toHour standardZone time
        , minute = Time.toMinute standardZone time
        , second = Time.toSecond standardZone time
        , millis = Time.toMillis standardZone time
        }


{-| Set year of [`Parts`](#Parts).

Year lower than `1970` will be treated as `1970`.

    epoch
        |> toParts utc
        |> withYear 1969
        |> fromParts utc
    --> epoch

-}
withYear : Int -> Parts -> Parts
withYear year (Parts parts) =
    let
        fixedYear =
            max 1970 year
    in
    Parts
        { parts
            | year = fixedYear
            , day = min parts.day (daysInMonth fixedYear parts.month)
        }


{-| Set month of [`Parts`](#Parts).
-}
withMonth : Time.Month -> Parts -> Parts
withMonth month (Parts parts) =
    Parts
        { parts
            | month = month
            , day = min parts.day (daysInMonth parts.year month)
        }


{-| Set day of [`Parts`](#Parts).
-}
withDay : Int -> Parts -> Parts
withDay day (Parts parts) =
    Parts { parts | day = clamp 1 (daysInMonth parts.year parts.month) day }


{-| Set hour of [`Parts`](#Parts).
-}
withHour : Int -> Parts -> Parts
withHour hour (Parts parts) =
    Parts { parts | hour = clamp 0 23 hour }


{-| Set minute of [`Parts`](#Parts).
-}
withMinute : Int -> Parts -> Parts
withMinute minute (Parts parts) =
    Parts { parts | minute = clamp 0 59 minute }


{-| Set second of [`Parts`](#Parts).
-}
withSecond : Int -> Parts -> Parts
withSecond second (Parts parts) =
    Parts { parts | second = clamp 0 59 second }


{-| Set millis of [`Parts`](#Parts).
-}
withMillis : Int -> Parts -> Parts
withMillis millis (Parts parts) =
    Parts { parts | millis = clamp 0 999 millis }


{-| Convert [`Parts`](#Parts) back to posix time.

    import Iso8601
    import Time


    -- Extract several eras from America/New_York for testing
    -- summer : 2022-03-13T02:00:00-05:00 -> 2022-03-13T03:00:00-04:00
    -- winter : 2022-11-06T02:00:00-04:00 -> 2022-11-06T01:00:00-05:00
    newYork : Zone
    newYork =
        customZone ""
            [ { start = 27976740, offset = -240 } -- 2023-03-12T03:00:00-04:00
            , { start = 27795240, offset = -300 } -- 2022-11-06T01:00:00-05:00
            , { start = 27452580, offset = -240 } -- 2022-03-13T03:00:00-04:00
            , { start = 27271080, offset = -300 } -- 2021-11-07T01:00:00-05:00
            ]
            -300

    -- Extract several eras from Europe/Paris for testing
    -- summer : 2022-03-27T02:00:00+01:00 -> 2022-03-27T03:00:00+02:00
    -- winter : 2022-10-30T03:00:00+02:00 -> 2022-10-30T02:00:00+01:00
    paris : Time2.Zone
    paris =
        customZone ""
            [ { start = 27996540, offset = 120 } -- 2023-03-26T03:00:00+02:00
            , { start = 27784860, offset = 60 } -- 2022-10-30T02:00:00+01:00
            , { start = 27472380, offset = 120 } -- 2022-03-27T03:00:00+02:00
            , { start = 27260700, offset = 60 } -- 2021-10-31T02:00:00+01:00
            ]
            60

    epoch
        |> toParts newYork
        |> withYear 2022
        |> withMonth Time.Mar
        |> withDay 13
        |> withHour 1
        |> withMinute 59
        |> withSecond 59
        |> fromParts newYork
        |> Ok
    --> Iso8601.toTime "2022-03-13T01:59:59-05:00"

    epoch
        |> toParts newYork
        |> withYear 2022
        |> withMonth Time.Mar
        |> withDay 13
        |> withHour 2
        |> fromParts newYork
        |> Ok
    --> Iso8601.toTime "2022-03-13T03:00:00-04:00"

    epoch
        |> toParts newYork
        |> withYear 2022
        |> withMonth Time.Mar
        |> withDay 13
        |> withHour 3
        |> fromParts newYork
        |> Ok
    --> Iso8601.toTime "2022-03-13T03:00:00-04:00"

    -- `fromParts` gives the second 01:59:59
    epoch
        |> toParts newYork
        |> withYear 2022
        |> withMonth Time.Nov
        |> withDay 6
        |> withHour 1
        |> withMinute 59
        |> withSecond 59
        |> fromParts newYork
        |> Ok
    --> Iso8601.toTime "2022-11-06T01:59:59-05:00"

    -- `fromParts` gives the second 2022-11-06T02:00:00
    epoch
        |> toParts newYork
        |> withYear 2022
        |> withMonth Time.Nov
        |> withDay 6
        |> withHour 2
        |> fromParts newYork
        |> Ok
    --> Iso8601.toTime "2022-11-06T02:00:00-05:00"

    epoch
        |> toParts paris
        |> withYear 2022
        |> withMonth Time.Mar
        |> withDay 27
        |> withHour 1
        |> withMinute 59
        |> withSecond 59
        |> fromParts paris
        |> Ok
    --> Iso8601.toTime "2022-03-27T01:59:59+01:00"

    epoch
        |> toParts paris
        |> withYear 2022
        |> withMonth Time.Mar
        |> withDay 27
        |> withHour 2
        |> fromParts paris
        |> Ok
    --> Iso8601.toTime "2022-03-27T03:00:00+02:00"

    epoch
        |> toParts paris
        |> withYear 2022
        |> withMonth Time.Mar
        |> withDay 27
        |> withHour 3
        |> fromParts paris
        |> Ok
    --> Iso8601.toTime "2022-03-27T03:00:00+02:00"

    -- `fromParts` gives the second 2022-10-30T02:59:59
    epoch
        |> toParts paris
        |> withYear 2022
        |> withMonth Time.Oct
        |> withDay 30
        |> withHour 2
        |> withMinute 59
        |> withSecond 59
        |> fromParts paris
        |> Ok
    --> Iso8601.toTime "2022-10-30T02:59:59+01:00"

    -- `fromParts` gives the second 2022-10-30T03:00:00
    epoch
        |> toParts paris
        |> withYear 2022
        |> withMonth Time.Oct
        |> withDay 30
        |> withHour 3
        |> fromParts paris
        |> Ok
    --> Iso8601.toTime "2022-10-30T03:00:00+01:00"

-}
fromParts : Zone -> Parts -> Time.Posix
fromParts zone parts =
    -- https://github.com/postgres/postgres/blob/2be87f092a2ac786264b2020797aafa837de5a8e/src/backend/utils/adt/datetime.c#L1655-L1790
    let
        myTime =
            partsToMillis parts

        { current, next } =
            Time.millisToPosix (myTime - millisPerDay)
                |> getEraTransitions zone

        beforeTime =
            myTime - current.offset * millisPerMinute
    in
    next
        |> Maybe.map
            (\after ->
                let
                    afterTime =
                        myTime - after.offset * millisPerMinute

                    boundary =
                        after.start * millisPerMinute
                in
                if beforeTime < boundary && afterTime < boundary then
                    Time.millisToPosix beforeTime

                else if beforeTime > boundary && afterTime >= boundary then
                    Time.millisToPosix afterTime

                else if beforeTime > afterTime then
                    Time.millisToPosix beforeTime

                else
                    Time.millisToPosix afterTime
            )
        |> Maybe.withDefault (Time.millisToPosix beforeTime)


partsToMillis : Parts -> Int
partsToMillis (Parts parts) =
    millisBeforeYear parts.year
        + millisBeforeMonth parts.year parts.month
        + ((parts.day - 1) * millisPerDay)
        + (parts.hour * millisPerHour)
        + (parts.minute * millisPerMinute)
        + (parts.second * 1000)
        + parts.millis


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0 && modBy 100 year /= 0) || (modBy 400 year == 0)


leapYearsBefore : Int -> Int
leapYearsBefore year =
    let
        year_ =
            year - 1
    in
    (year_ // 4) - (year_ // 100) + (year_ // 400)


millisBeforeYear : Int -> Int
millisBeforeYear year =
    let
        days =
            (year - 1970)
                * 365
                + leapYearsBefore year
                - leapYearsBefore 1970
    in
    days * millisPerDay


millisBeforeMonth : Int -> Time.Month -> Int
millisBeforeMonth year month =
    if isLeapYear year && month /= Time.Jan && month /= Time.Feb then
        millisBeforeMonthHelper month + millisPerDay

    else
        millisBeforeMonthHelper month


millisBeforeMonthHelper : Time.Month -> Int
millisBeforeMonthHelper month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            2678400000

        Time.Mar ->
            5097600000

        Time.Apr ->
            7776000000

        Time.May ->
            10368000000

        Time.Jun ->
            13046400000

        Time.Jul ->
            15638400000

        Time.Aug ->
            18316800000

        Time.Sep ->
            20995200000

        Time.Oct ->
            23587200000

        Time.Nov ->
            26265600000

        Time.Dec ->
            28857600000


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


millisPerDay : number
millisPerDay =
    86400000


millisPerHour : number
millisPerHour =
    3600000


millisPerMinute : number
millisPerMinute =
    60000


getEraTransitions : Zone -> Time.Posix -> { current : Era, next : Maybe Era }
getEraTransitions (Zone { eras, offsetOfEarliestEra }) time =
    let
        minutes =
            Time.posixToMillis time // millisPerMinute

        helper previousEra remainingEras =
            case remainingEras of
                [] ->
                    { current = { start = 0, offset = offsetOfEarliestEra }, next = previousEra }

                head :: tail ->
                    if head.start <= minutes then
                        { current = head, next = previousEra }

                    else
                        helper (Just head) tail
    in
    helper Nothing eras
