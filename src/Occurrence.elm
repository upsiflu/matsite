module Occurrence exposing
    ( Occurrence
    , encode, decode
    , moment
    , withDurationDays, withDurationMinutes
    , merge
    , bounds
    , ViewMode(..), view
    , edit
    )

{-| This requires two packages, one to calculate calendar dates and one to calculate hours,
timezones and canonical format.

We are storing all dates as POSIX for interoperability, and add precision markers for
improved humaneness.

@docs Occurrence
@docs encode, decode


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

import DateFormat exposing (format)
import DateTime exposing (DateTime)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import String.Extra as String
import Time exposing (Month(..), Posix, Zone)


{-| An event has a single Occurrence which may comprise Occasions of varying precision.

Note that all dates are stored in UTC and will be converted to the local timezone in the view!

-}
type alias Occurrence =
    List Occasion


{-| Zero-duration occurrence
-}
moment : Time.Zone -> Time.Month -> Int -> Int -> Int -> Int -> Occurrence
moment zone month day year hour minute =
    DateTime.fromRawParts { day = day, month = month, year = year } { hours = hour, minutes = minute, seconds = 0, milliseconds = 0 }
        |> Maybe.map
            (localToGlobal zone
                >> (\posix -> [ ( posix, posix ) ])
            )
        |> Maybe.withDefault []


{-| Discards the end dates in all `Occasion`s
-}
withDurationMinutes : Int -> Occurrence -> Occurrence
withDurationMinutes duration =
    List.map
        (\( from, _ ) ->
            ( from, Time.millisToPosix (Time.posixToMillis from + duration * 60 * 1000) )
        )


{-| Discards the end dates in all `Occasion`s
-}
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


decodeOccasion : Decoder Occasion
decodeOccasion =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodePosix)
        (Decode.field "A2" decodePosix)


{-| -}
decode : Decoder Occurrence
decode =
    Decode.list decodeOccasion


encodeOccasion : Occasion -> Value
encodeOccasion ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodePosix a1 )
        , ( "A2", encodePosix a2 )
        ]


encodePosix : Posix -> Value
encodePosix =
    Time.posixToMillis >> toFloat >> (\x -> x / 1000) >> Encode.float


decodePosix : Decoder Posix
decodePosix =
    Decode.map (truncate >> (*) 1000 >> Time.millisToPosix) Decode.float


{-| -}
encode : Occurrence -> Value
encode a =
    Encode.list encodeOccasion a


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
    let
        sortTime listSort =
            List.map Time.posixToMillis >> listSort >> Maybe.withDefault 0 >> Time.millisToPosix
    in
    List.unzip
        >> Tuple.mapBoth
            (sortTime List.minimum)
            (sortTime List.maximum)


{-| -}
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
                    _ :: _ ->
                        let
                            beginning =
                                occurrence
                                    |> List.map (Tuple.first >> Time.posixToMillis)
                                    |> List.minimum
                                    |> Maybe.withDefault 0
                                    |> Time.millisToPosix

                            ending =
                                occurrence
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


replace : Occasion -> Occasion -> Occurrence -> Occurrence
replace oldOccasion =
    List.setIf ((==) oldOccasion >> Debug.log "replace here")


replaceAt : Int -> Occasion -> Occurrence -> Occurrence
replaceAt =
    List.setAt


addDefaultOccasion : Occurrence -> Occurrence
addDefaultOccasion occurrence =
    let
        addWeek : Time.Posix -> Time.Posix
        addWeek =
            Time.posixToMillis
                >> (+) (7 * 24 * 60 * 60 * 1000)
                >> Time.millisToPosix

        oneWeekLater : Occasion -> Occasion
        oneWeekLater =
            Tuple.mapBoth addWeek addWeek
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

        makeEditable i ( from, until ) =
            [ Html.details [ class "edit" ]
                [ Html.summary []
                    [ occasionToString zoneData Minutes ( from, until ) |> Html.text
                    ]
                , Html.button [ title "remove Occasion", class "remove", onClick (List.remove ( from, until ) occurrence |> save) ] [ Html.text "⌫" ]
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
                                replaceAt i ( toPosix newFrom |> Maybe.withDefault from, until ) occurrence |> save
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
                                replaceAt i ( from, toPosix newUntil |> Maybe.withDefault until ) occurrence |> save
                            )
                        , value (fromPosix until |> Debug.log "datetime local")
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
