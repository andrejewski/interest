module Main exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (type_, href, class, classList, defaultValue)
import Html.Events exposing (..)
import Svg exposing (svg, path)
import Svg.Attributes exposing (d, fill)


type alias Model =
    { yearDollarCost : Float
    , yearDollarCostInput : String
    , isYearDollarCostValid : Bool
    , interestRatePercent : Float
    , interestRatePercentInput : String
    , isInterestRatePercentValid : Bool
    , compoundRate : CompoundRate
    }


type CompoundRate
    = Day
    | Week
    | Month
    | Year


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialYearDollarCost : Float
initialYearDollarCost =
    50000


initialInterestRatePercent : Float
initialInterestRatePercent =
    8


fixed2 : String -> String
fixed2 str =
    let
        parts =
            Array.fromList (String.split "." str)

        lastIndex =
            Array.length parts - 1

        last =
            Array.get lastIndex parts
    in
        if lastIndex > 0 then
            case last of
                Nothing ->
                    str

                Just end ->
                    let
                        formattedEnd =
                            end |> String.left 2 |> String.padRight 2 '0'

                        wholePart =
                            parts
                                |> Array.slice 0 -1
                                |> Array.toList
                                |> String.join "."
                                |> commaDollar
                    in
                        wholePart ++ "." ++ formattedEnd
        else
            commaDollar str


commaDollar : String -> String
commaDollar str =
    let
        lastThree =
            String.right 3 str

        first =
            String.dropRight 3 str

        isFinal =
            (String.length lastThree < 3) || (String.length first == 0)
    in
        if isFinal then
            lastThree
        else
            (commaDollar first) ++ "," ++ lastThree


floatToDollarAmount : Float -> String
floatToDollarAmount dollars =
    dollars
        |> toString
        |> fixed2
        |> (++) "$"


stripDollarSymbol : String -> String
stripDollarSymbol dollars =
    if String.startsWith "$" dollars then
        String.dropLeft 1 dollars
    else
        dollars


dollarAmountToFloat : String -> Result String Float
dollarAmountToFloat dollars =
    dollars
        |> String.trim
        |> stripDollarSymbol
        |> String.split ","
        |> List.map String.trim
        |> String.join ""
        |> String.toFloat


floatToPercentage : Float -> String
floatToPercentage percent =
    toString percent ++ "%"


stripPercentSymbol : String -> String
stripPercentSymbol percent =
    if String.endsWith "%" percent then
        String.dropRight 1 percent
    else
        percent


percentageToFloat : String -> Result String Float
percentageToFloat percent =
    percent
        |> String.trim
        |> stripPercentSymbol
        |> String.toFloat


init : ( Model, Cmd Msg )
init =
    ( { yearDollarCost = initialYearDollarCost
      , yearDollarCostInput = floatToDollarAmount initialYearDollarCost
      , isYearDollarCostValid = True
      , interestRatePercent = initialInterestRatePercent
      , interestRatePercentInput = floatToPercentage initialInterestRatePercent
      , isInterestRatePercentValid = True
      , compoundRate = Month
      }
    , Cmd.none
    )


type Msg
    = SetYearDollarCost String
    | SetInterestRatePercent String
    | SetCompoundRate CompoundRate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetYearDollarCost yearDollarCost ->
            let
                parseResult =
                    dollarAmountToFloat yearDollarCost

                newModel =
                    { model | yearDollarCostInput = yearDollarCost }
            in
                case parseResult of
                    Ok value ->
                        ( { newModel | yearDollarCost = value, isYearDollarCostValid = True }, Cmd.none )

                    Err _ ->
                        ( { newModel | isYearDollarCostValid = False }, Cmd.none )

        SetInterestRatePercent interestRatePercent ->
            let
                parseResult =
                    percentageToFloat interestRatePercent

                newModel =
                    { model | interestRatePercentInput = interestRatePercent }
            in
                case parseResult of
                    Ok value ->
                        ( { newModel | interestRatePercent = value, isInterestRatePercentValid = True }, Cmd.none )

                    Err _ ->
                        ( { newModel | isInterestRatePercentValid = False }, Cmd.none )

        SetCompoundRate compoundRate ->
            ( { model | compoundRate = compoundRate }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [ class "main" ]
            [ formView model
            , visualView model
            ]
        , footer []
            [ hr [] []
            , p []
                [ text "Made by "
                , a [ href "https://jew.ski/" ] [ text "Chris Andrejewski" ]
                , text ", view "
                , a [ href "https://github.com/andrejewski/interest" ] [ text "source code" ]
                ]
            ]
        ]


formView : Model -> Html Msg
formView model =
    let
        currentYearCost =
            span []
                [ model.yearDollarCost
                    |> floatToDollarAmount
                    |> String.append "Showing "
                    |> text
                ]

        yearCostPieces =
            if model.isYearDollarCostValid then
                [ currentYearCost ]
            else
                [ span [ class "error-text" ] [ text "Invalid dollar amount" ]
                , div [ class "breaker" ] []
                , currentYearCost
                ]

        currentInterestRate =
            span []
                [ model.interestRatePercent
                    |> floatToPercentage
                    |> String.append "Showing "
                    |> text
                ]

        interestRatePieces =
            if model.isInterestRatePercentValid then
                [ currentInterestRate ]
            else
                [ span [ class "error-text" ] [ text "Invalid interest rate percentage" ]
                , div [ class "breaker" ] []
                , currentInterestRate
                ]
    in
        form []
            [ div [ class "title" ]
                [ h1 [] [ text "Interest" ]
                , p [] [ text "Calculate the principal amount of money needed to achieve a self-sustaining revenue stream from compound interest." ]
                ]
            , label []
                [ span [] [ text "Expected yearly withdrawal" ]
                , input
                    [ type_ "text"
                    , class "number"
                    , onInput SetYearDollarCost
                    , defaultValue model.yearDollarCostInput
                    ]
                    []
                , small [] yearCostPieces
                ]
            , label []
                [ span []
                    [ text "Interest Rate" ]
                , input
                    [ type_ "text"
                    , class "number"
                    , onInput SetInterestRatePercent
                    , defaultValue model.interestRatePercentInput
                    ]
                    []
                , small [] interestRatePieces
                ]
            , label []
                [ span []
                    [ text "Compound every" ]
                , div [ class "tabs" ]
                    [ span [ classList [ ( "active", model.compoundRate == Day ) ], onClick (SetCompoundRate Day) ] [ text "Day" ]
                    , span [ classList [ ( "active", model.compoundRate == Week ) ], onClick (SetCompoundRate Week) ] [ text "Week" ]
                    , span [ classList [ ( "active", model.compoundRate == Month ) ], onClick (SetCompoundRate Month) ] [ text "Month" ]
                    , span [ classList [ ( "active", model.compoundRate == Year ) ], onClick (SetCompoundRate Year) ] [ text "Year" ]
                    ]
                ]
            ]


inf : Float
inf =
    1 / 0


growthGraph : List Float -> Html Msg
growthGraph points =
    let
        width =
            300

        height =
            300

        minimum =
            List.foldl min inf points

        maximum =
            List.foldl max -inf points

        valueRange =
            maximum - minimum

        middlePoints =
            points

        adjustedPoints =
            middlePoints
                |> List.map
                    (\t ->
                        t
                            |> (\t -> t - minimum)
                            |> (\t -> t / valueRange)
                            |> (*) height
                    )

        pointCount =
            List.length middlePoints

        widthInterval =
            width / toFloat (pointCount - 1)

        svgPoints =
            adjustedPoints
                |> List.indexedMap
                    (\i p ->
                        ( widthInterval * (toFloat i)
                        , height - p
                        )
                    )

        pathMiddle =
            svgPoints
                |> List.map (\( w, h ) -> "L " ++ toString w ++ " " ++ toString h)
                |> String.join " "

        pathStart =
            "M 300 300 "

        pathEnd =
            " L 300 0 Z"

        dataPath =
            pathStart ++ pathMiddle ++ pathEnd
    in
        svg
            [ Svg.Attributes.width "300"
            , Svg.Attributes.height "300"
            ]
            [ path
                [ d dataPath
                , fill "#def"
                ]
                []
            ]


compoundRateToIntervals : CompoundRate -> Float
compoundRateToIntervals compoundRate =
    case compoundRate of
        Day ->
            356

        Week ->
            52

        Month ->
            12

        Year ->
            1


growthCurve : Model -> List Float
growthCurve model =
    let
        principal =
            growthPrincipal model

        timesCompoundedPerYear =
            compoundRateToIntervals model.compoundRate

        interestRate =
            model.interestRatePercent / 100
    in
        List.range 0 (round timesCompoundedPerYear)
            |> List.map (\i -> compound principal interestRate timesCompoundedPerYear (toFloat i * (1 / timesCompoundedPerYear)))


growthPrincipal : Model -> Float
growthPrincipal model =
    findPrincipal
        model.yearDollarCost
        (compoundRateToIntervals model.compoundRate)
        (model.interestRatePercent / 100)
        1


compound : Float -> Float -> Float -> Float -> Float
compound principal interestRate timesCompoundedPerYear timeInYears =
    principal * ((1 + (interestRate / timesCompoundedPerYear)) ^ (timesCompoundedPerYear * timeInYears))


findPrincipal : Float -> Float -> Float -> Float -> Float
findPrincipal e n r t =
    e / (((1 + (r / n)) ^ (n * t)) - 1)


visualView : Model -> Html Msg
visualView model =
    div [ class "visual" ]
        [ div [ class "block" ] [ model |> growthCurve |> growthGraph ]
        , div [ class "bottom" ] [ p [] [ model |> growthPrincipal |> floatToDollarAmount |> text ] ]
        ]
