module DataEntry exposing (..)

import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (..)
import Html.Attributes exposing (..)


type Statistic = NotSelected | Total | Mean

type Visibility
    = Hidden
    | Shown


type EntryState
    = Blank
    | Correct
    | NotANumber
    | OutOfBounds
    | OtherwiseIncorrect


type alias NumericData a =
    { str : String
    , val : Maybe a
    , state : EntryState
    }


initInt : NumericData Int
initInt =
    { str = ""
    , val = Nothing
    , state = Blank
    }


type alias LblData =
    { str : String
    , state : EntryState
    }


initLbL : LblData
initLbL =
    { str = "", state = Blank }


type alias EntryConfig msg =
    { placeholder : String
    , label : String
    , onInput : String -> msg
    }


entryConfig : String -> String -> (String -> msg) -> EntryConfig msg
entryConfig placeholder label onInput =
    { placeholder = placeholder
    , label = label
    , onInput = onInput
    }

baseOptions : String -> (String -> msg) -> List (Input.Option msg)
baseOptions placeholder msg =
            [ Input.placeholder placeholder
            , Input.onInput msg
            ]

addBaseOptions : String -> (String -> msg) -> List (Html.Attribute msg) -> List (Input.Option msg)
addBaseOptions placeholder msg htmlOpts =
            [ Input.attrs htmlOpts
            , Input.placeholder placeholder
            , Input.onInput msg
            ]


baseHtmlAttrs : String -> List (Html.Attribute msg)
baseHtmlAttrs name =
    [ Html.Attributes.name name
    , Html.Attributes.id name
    ]

addTabIndex : Int -> List (Html.Attribute msg) -> List (Html.Attribute msg)
addTabIndex n opts =
            Html.Attributes.tabindex n  :: opts

toInputOptions : List (Html.Attribute msg) -> List (Input.Option msg)
toInputOptions htmlOpts =
    [Input.attrs htmlOpts]

withValue : String -> List (Input.Option msg) -> List (Input.Option msg)
withValue value opts =
            (Input.value value) :: opts

addEntryState : EntryState -> List (Input.Option msg) -> List (Input.Option msg)
addEntryState status opts =
    case status of
        Blank ->
            opts

        Correct ->
            Input.success :: opts

        _ ->
            Input.danger :: opts


numericEntryState : (String -> Maybe a) -> (a -> Bool) -> String -> EntryState
numericEntryState convert isOutOfBounds input =
    case ( input, convert input ) of
        ( "", _ ) ->
            Blank

        ( _, Nothing ) ->
            NotANumber

        ( _, Just p ) ->
            if isOutOfBounds p then 
               Correct 

           else 
               OutOfBounds


updateNumericStr : (String -> Maybe a) -> String -> NumericData a -> NumericData a
updateNumericStr convert input numbericData =
    { numbericData
        | str = input
        , val = convert input
    }


updateNumericState : (String -> Maybe a) -> (a -> Bool) -> NumericData a -> NumericData a
updateNumericState convert isOutOfBounds numbericData =
    { numbericData
        | state = numericEntryState convert isOutOfBounds numbericData.str
    }


updateNumeric : (String -> Maybe number) -> (number -> Bool) -> String-> NumericData number -> NumericData number
updateNumeric convert isOutOfBounds input numbericData =
    numbericData
        |> updateNumericStr convert input
        |> updateNumericState convert isOutOfBounds


entryView : String -> List (Input.Option msg) -> Html msg
entryView label opts =
    InputGroup.config
        (InputGroup.text opts)
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ Html.text label ] ]
        |> InputGroup.view


errorView : (a -> Bool) -> String -> a -> Html msg
errorView hasError msg model =
    let
        isInError =
            hasError model
    in
      if isInError then
              Html.span [ Html.Attributes.style "color" "red" ] [ Html.text msg ]
      else
              Html.span [] [ Html.text "" ]


-- Data Entry for X--The limit of a p value

isXInOfBounds : Float -> Bool
isXInOfBounds x = (x >= 0)


updateXData lbl model = 
    updateNumeric String.toFloat isXInOfBounds lbl model.xData


makeHtmlText : String -> String -> Html msg
makeHtmlText header str =
    Html.text (header ++ str)

-- view helpers

basicEntry lbl placeholder tab msg state = 
    entryView lbl   (baseHtmlAttrs placeholder
                    |> addTabIndex tab
                    |> addBaseOptions placeholder msg
                    |> addEntryState state
                    )

eventEntry = basicEntry "Event" "Label" 1
intervalEntry = basicEntry "Interval" "Label" 2
lambdaEntry = basicEntry "\u{1D706}" "" 3
nEntry = basicEntry "n" "" 4


hasLabelError : { a | eventLbl : LblData, intervalLbl : LblData} -> Bool
hasLabelError model  =
    (model.eventLbl.state == OtherwiseIncorrect) || (model.intervalLbl.state == OtherwiseIncorrect)


labelError = errorView hasLabelError "The labels cannot be the same."


hasLambdaError model =
    (model.lambdaData.state == NotANumber) || (model.lambdaData.state == OutOfBounds)


lambdaError = errorView hasLambdaError "λ is a number strictly bigger than 0."


hasNError model =
    (model.nData.state == NotANumber) || (model.nData.state == OutOfBounds)


nError = errorView hasNError "n is a whole number."


xEntry msg model =
    let
        lbl = 
            case model.statistic of
                Mean ->
                    "mean"

                _ ->
                    "total"
    in
        entryView lbl (baseHtmlAttrs lbl
                        |> addBaseOptions "" msg
                        |> withValue model.xData.str
                        |> addEntryState model.xData.state
                        |> withValue model.xData.str
                        )

hasXError model =
    (model.xData.state == NotANumber) || (model.xData.state == OutOfBounds)

xError model =
    let
        msg = "The mean is a number strictly greater than 0"
    in
        errorView hasXError msg model

