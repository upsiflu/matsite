module Occurrence exposing
    ( Occurrence
    , codec
    , moment
    , withDurationDays, withDurationMinutes
    , merge
    , bounds
    , ViewMode(..), view
    , Precision(..), beginning, edit, sort, toString
    )

{-| This requires two packages, one to calculate calendar dates and one to calculate hours,
timezones and canonical format.

We are storing all dates as POSIX for interoperability, and add precision markers for
improved humaneness.

@docs Occurrence
@docs codec


# Create

@docs moment


# Modify

@docs withDurationDays, withDurationMinutes


# Combine

@docs merge


# Deconstruct

@docs bounds


# View

@docs ViewMode, view

-}

import Codec exposing (Codec)
import DateFormat exposing (format)
import DateTime exposing (DateTime)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Iso8601
import List.Extra as List
import String.Extra as String
import Time exposing (Month(..), Zone)


{-| An event has a single Occurrence which may comprise Occasions of varying precision.

Note that all dates are stored in UTC and will be converted to the local timezone in the view!

-}
type alias Occurrence =
    List Occasion


{-| -}
codec : Codec Occurrence
codec =
    Codec.list occasionCodec


{-| Zero-duration occurrence
-}
moment : Time.Zone -> Time.Month -> Int -> Int -> Int -> Int -> Occurrence
moment zone month day year hour minute =
    DateTime.fromRawParts { day = day, month = month, year = year } { hours = hour, minutes = minute, seconds = 0, milliseconds = 0 }
        |> Maybe.map
            (localToGlobal zone
                >> (\posix -> [ { from = posix, until = posix } ])
            )
        |> Maybe.withDefault []


{-| Discards the end dates in all `Occasion`s
-}
withDurationMinutes : Int -> Occurrence -> Occurrence
withDurationMinutes duration =
    List.map <|
        \occurrence ->
            { occurrence | until = Time.millisToPosix (Time.posixToMillis occurrence.from + duration * 60 * 1000) }


{-| Discards the end dates in all `Occasion`s
-}
withDurationDays : Int -> Occurrence -> Occurrence
withDurationDays duration =
    List.map <|
        \occurrence ->
            { occurrence | until = Time.millisToPosix (Time.posixToMillis occurrence.from + duration * 24 * 60 * 60 * 1000) }


{-| Examples:

    singleDay 2019 Aug 26

    timespan ( 2019 Aug 26, 15 35, 0 + 90 )

-}
type alias Occasion =
    { from : Time.Posix, until : Time.Posix }


occasionCodec : Codec Occasion
occasionCodec =
    Codec.object Occasion
        |> Codec.field "from" .from posixCodec
        |> Codec.field "until" .until posixCodec
        |> Codec.buildObject


posixCodec : Codec Time.Posix
posixCodec =
    Codec.map Time.millisToPosix Time.posixToMillis Codec.int


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


toString : ( String, Zone ) -> Precision -> Occurrence -> String
toString ( zoneDesc, zone ) precision =
    List.map (occasionToString zone precision)
        >> String.join " + "
        >> (\str -> str ++ " (in your current timezone, " ++ zoneDesc ++ ")")


beginning : Occurrence -> Maybe Time.Posix
beginning =
    List.head >> Maybe.map .from



---- Combine


{-| `merge = (++)`
-}
merge : Occurrence -> Occurrence -> Occurrence
merge =
    (++)



---- Destructure


{-| the first beginning and last end time
-}
bounds : Occurrence -> Occasion
bounds =
    List.foldl1
        (\{ from, until } acc ->
            { from =
                if Time.posixToMillis from < Time.posixToMillis acc.from then
                    from

                else
                    acc.from
            , until =
                if Time.posixToMillis until > Time.posixToMillis acc.until then
                    until

                else
                    acc.until
            }
        )
        >> Maybe.withDefault { from = Time.millisToPosix 0, until = Time.millisToPosix 0 }


{-| `AsList` : Show all
`Short` : Show bounds
-}
type ViewMode
    = AsList Time.Zone Precision
    | Short Time.Zone Precision


{-| -}
view : ViewMode -> Occurrence -> Html Never
view mode =
    case mode of
        AsList zone precision ->
            List.map (occasionToString zone precision >> Html.text >> List.singleton >> Html.li [])
                >> Html.ul []

        Short zone precision ->
            \occurrence ->
                case occurrence of
                    [] ->
                        Html.text "--"

                    _ :: _ ->
                        bounds occurrence
                            |> occasionToString zone precision
                            |> Html.text


occasionToString : Zone -> Precision -> Occasion -> String
occasionToString zone precision { from, until } =
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
                    , DateFormat.dayOfMonthNumber
                    ]
                    zone
                    from
                    ++ format
                        [ DateFormat.text " + "
                        , DateFormat.dayOfMonthNumber
                        , DateFormat.text " '"
                        , DateFormat.yearNumberLastTwo
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


replaceAt : Int -> Occasion -> Occurrence -> Occurrence
replaceAt =
    List.setAt


addDefaultOccasion : Occurrence -> Occurrence
addDefaultOccasion occurrence =
    let
        addWeeks : Int -> Time.Posix -> Time.Posix
        addWeeks howMany =
            Time.posixToMillis
                >> (+) (howMany * 7 * 24 * 60 * 60 * 1000)
                >> Time.millisToPosix

        oneWeekLater : Occasion -> Occasion
        oneWeekLater { from, until } =
            { from = addWeeks 2 from, until = addWeeks 2 until }
    in
    case List.reverse occurrence of
        [] ->
            moment Time.utc Time.Aug 1 2022 18 0
                |> withDurationMinutes 90

        latest :: earlier ->
            List.reverse (oneWeekLater latest :: latest :: earlier)


edit : { zone : ( String, Time.Zone ), save : Occurrence -> msg } -> Occurrence -> Html msg
edit { zone, save } occurrence =
    let
        ( zoneName, zoneData ) =
            zone

        toPosix : String -> Maybe Time.Posix
        toPosix =
            (\str -> str ++ ":00.000Z")
                >> Iso8601.toTime
                >> Result.toMaybe
                >> Maybe.map
                    (\localPosix ->
                        Time.posixToMillis localPosix
                            - DateTime.getTimezoneOffset zoneData localPosix
                            |> Time.millisToPosix
                    )

        fromPosix : Time.Posix -> String
        fromPosix =
            (\localPosix ->
                Time.posixToMillis localPosix
                    + DateTime.getTimezoneOffset zoneData localPosix
                    |> Time.millisToPosix
            )
                >> Iso8601.fromTime
                >> String.leftOfBack ":"

        additionalOccasion =
            [ Html.button [ title "add Occasion", class "add", onClick (save (addDefaultOccasion occurrence)) ] [ Html.text "+" ] ]

        makeEditable i ({ from, until } as occasion) =
            [ Html.details [ class "edit" ]
                [ Html.summary []
                    [ occasionToString zoneData Minutes occasion |> Html.text
                    ]
                , Html.button [ title "remove Occasion", class "remove", onClick (List.remove occasion occurrence |> save) ] [ Html.text "⌫" ]
                , Html.label []
                    [ " (as shown in your Timezone " ++ zoneName ++ ")" |> Html.text
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "from" ]
                    , Html.input
                        [ type_ "datetime-local"
                        , value (fromPosix from)
                        , onInput
                            (\newFrom ->
                                replaceAt i { occasion | from = toPosix newFrom |> Maybe.withDefault from } occurrence |> save
                            )
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "until" ]
                    , Html.input
                        [ type_ "datetime-local"
                        , onInput
                            (\newUntil ->
                                replaceAt i { occasion | until = toPosix newUntil |> Maybe.withDefault until } occurrence |> save
                            )
                        , value (fromPosix until)
                        ]
                        []
                    ]
                ]
            ]
                |> Html.li [ class "occasion" ]
    in
    List.indexedMap makeEditable occurrence
        |> Html.ul [ class "occasions" ]
        |> (\list -> Html.div [ class "dates" ] (list :: additionalOccasion))


sort : Occurrence -> Occurrence
sort =
    List.sortBy (.from >> Time.posixToMillis)
