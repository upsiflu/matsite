module Occurrence exposing (..)

{-| This requires two packages, one to calculate calendar dates and one to calculate hours,
timezones and canonical format.

We are storing all dates as POSIX for interoperability, and add precision markers for
improved humaneness.

-}

import DateFormat exposing (format)
import DateTime exposing (DateTime)
import Html.Styled as Html exposing (Html)
import Time exposing (Month(..), Zone)


{-| An event has a single Occurrence which may comprise Occasions of varying precision.

Note that all dates are stored in UTC and will be converted to the local timezone in the view!

-}
type alias Occurrence =
    List Occasion


moment : Time.Zone -> Time.Month -> Int -> Int -> Int -> Int -> Occurrence
moment zone month day year hour minute =
    DateTime.fromRawParts { day = day, month = month, year = year } { hours = hour, minutes = minute, seconds = 0, milliseconds = 0 }
        |> Maybe.map
            (localToGlobal zone
                >> (\posix -> [ ( posix, posix ) ])
            )
        |> Maybe.withDefault []


withDurationMinutes : Int -> Occurrence -> Occurrence
withDurationMinutes duration =
    List.map
        (\( from, _ ) ->
            ( from, Time.millisToPosix (Time.posixToMillis from + duration * 60 * 1000) )
        )


withDurationDays : Int -> Occurrence -> Occurrence
withDurationDays duration =
    List.map
        (\( from, _ ) ->
            ( from, Time.millisToPosix (Time.posixToMillis from + duration * 24 * 60 * 60 * 1000) )
        )


{-| Examples:

    singleDay 2019 Aug 26

    timespan ( 2019 Aug 26, 15 35, 0 + 90 )

-}
type alias Occasion =
    ( Time.Posix, Time.Posix )


type Precision
    = Years
    | Days
    | Minutes


{-| For date arithmetic, we add the timezone offset, but we will never transform back to UTC
-}
localDateTime : Zone -> Time.Posix -> DateTime
localDateTime zone posix =
    Time.posixToMillis posix
        + DateTime.getTimezoneOffset zone posix
        |> Time.millisToPosix
        |> DateTime.fromPosix


{-| For the database, we subtract the timezone offset.
-}
localToGlobal : Zone -> DateTime -> Time.Posix
localToGlobal zone local =
    DateTime.toPosix local
        |> (\localPosix ->
                Time.posixToMillis localPosix - DateTime.getTimezoneOffset zone localPosix
           )
        |> Time.millisToPosix


toString : Zone -> Precision -> Occurrence -> String
toString zone precision =
    List.map (occasionToString zone precision)
        >> String.join ", "


type ViewMode
    = AsList Time.Zone Precision
    | Short Time.Zone Precision


view : ViewMode -> Occurrence -> Html Never
view mode =
    case mode of
        AsList zone precision ->
            List.map (occasionToString zone precision >> Html.text >> List.singleton >> Html.li [])
                >> Html.ul []

        Short zone precision ->
            \occurance ->
                case occurance of
                    _ :: _ ->
                        let
                            beginning =
                                occurance
                                    |> List.map (Tuple.first >> Time.posixToMillis)
                                    |> List.minimum
                                    |> Maybe.withDefault 0
                                    |> Time.millisToPosix

                            ending =
                                occurance
                                    |> List.map (Tuple.second >> Time.posixToMillis)
                                    |> List.maximum
                                    |> Maybe.withDefault 0
                                    |> Time.millisToPosix
                        in
                        ( beginning, ending )
                            |> occasionToString zone precision
                            |> Html.text

                    [] ->
                        Html.text "--"


occasionToString : Zone -> Precision -> Occasion -> String
occasionToString zone precision ( from, until ) =
    let
        ( localFrom, localUntil ) =
            ( localDateTime zone from, localDateTime zone until )

        isSameTime =
            Time.posixToMillis from - Time.posixToMillis until == 0

        isSameDay =
            DateTime.getDayDiff localFrom localUntil == 0

        isSubsequentDay =
            DateTime.getDayDiff localFrom localUntil
                == 1
                && isSameMonth
                && isSameYear

        isSubsequentYear =
            DateTime.getYear localFrom + 1 == DateTime.getYear localUntil

        isSameMonth =
            isSameYear && DateTime.getMonth localFrom == DateTime.getMonth localUntil

        isSameYear =
            DateTime.getYear localFrom == DateTime.getYear localUntil
    in
    case precision of
        Years ->
            if isSameYear then
                format
                    [ DateFormat.yearNumber ]
                    zone
                    from

            else if isSubsequentYear then
                format
                    [ DateFormat.yearNumber ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " + "
                        , DateFormat.yearNumber
                        ]
                        zone
                        until

            else
                format
                    [ DateFormat.yearNumber ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " - "
                        , DateFormat.yearNumber
                        ]
                        zone
                        until

        Days ->
            if isSameDay then
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    , DateFormat.text ", "
                    , DateFormat.yearNumber
                    ]
                    zone
                    from

            else if isSubsequentDay then
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " + "
                        , DateFormat.dayOfMonthSuffix
                        , DateFormat.text ", "
                        , DateFormat.yearNumber
                        ]
                        zone
                        until

            else if isSameMonth then
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " - "
                        , DateFormat.dayOfMonthSuffix
                        , DateFormat.text ", "
                        , DateFormat.yearNumber
                        ]
                        zone
                        until

            else
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    , DateFormat.text ", "
                    , DateFormat.yearNumber
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " - "
                        , DateFormat.monthNameAbbreviated
                        , DateFormat.text " "
                        , DateFormat.dayOfMonthSuffix
                        , DateFormat.text ", "
                        , DateFormat.yearNumber
                        ]
                        zone
                        until

        Minutes ->
            if isSameTime then
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    , DateFormat.text ", "
                    , DateFormat.yearNumber
                    , DateFormat.text ", "
                    , DateFormat.hourMilitaryFixed
                    , DateFormat.text ":"
                    , DateFormat.minuteFixed
                    ]
                    zone
                    from

            else if isSameDay then
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    , DateFormat.text ", "
                    , DateFormat.yearNumber
                    , DateFormat.text ", "
                    , DateFormat.hourMilitaryFixed
                    , DateFormat.text ":"
                    , DateFormat.minuteFixed
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " - "
                        , DateFormat.hourMilitaryFixed
                        , DateFormat.text ":"
                        , DateFormat.minuteFixed
                        ]
                        zone
                        until

            else if isSameYear then
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    , DateFormat.text ", "
                    , DateFormat.hourMilitaryFixed
                    , DateFormat.text ":"
                    , DateFormat.minuteFixed
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " - "
                        , DateFormat.monthNameAbbreviated
                        , DateFormat.text " "
                        , DateFormat.dayOfMonthSuffix
                        , DateFormat.text ", "
                        , DateFormat.hourMilitaryFixed
                        , DateFormat.text ":"
                        , DateFormat.minuteFixed
                        , DateFormat.text "; "
                        , DateFormat.yearNumber
                        ]
                        zone
                        until

            else
                format
                    [ DateFormat.monthNameAbbreviated
                    , DateFormat.text " "
                    , DateFormat.dayOfMonthSuffix
                    , DateFormat.text ", "
                    , DateFormat.yearNumber
                    , DateFormat.text "; "
                    , DateFormat.hourMilitaryFixed
                    , DateFormat.text ":"
                    , DateFormat.minuteFixed
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " - "
                        , DateFormat.monthNameAbbreviated
                        , DateFormat.text " "
                        , DateFormat.dayOfMonthSuffix
                        , DateFormat.text ", "
                        , DateFormat.yearNumber
                        , DateFormat.text "; "
                        , DateFormat.hourMilitaryFixed
                        , DateFormat.text ":"
                        , DateFormat.minuteFixed
                        ]
                        zone
                        until
