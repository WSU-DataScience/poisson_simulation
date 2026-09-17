module SingleObservation exposing (..)


import DataEntry exposing (..)
import Layout exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


-- This pages main

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model = { eventLbl : LblData
                   , intervalLbl : LblData
                   , lambdaData : NumericData Float
                   , nData : NumericData Int
                   , pulldown : Dropdown.State
                   , statistic : Statistic
                   }

-- Initialize

initFloat : NumericData Float
initFloat = {str = "", val = Nothing, state = Blank}

initLbl : LblData
initLbl = {str = "", state = Blank}

initModel = { eventLbl = initLbl
            , intervalLbl = initLbl
            , lambdaData = initFloat
            , nData = initInt
            , pulldown = Dropdown.initialState
            , statistic = NotSelected
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- Messages

type Msg  = ChangeEventLbl String
          | ChangeIntervalLbl String
          | ChangeLambda String
          | ChangeN String
          | ChangePulldown Dropdown.State
          | UseTotal
          | UseMean


-- update functions

labelState thisLbl otherLbl =
    if thisLbl == "" then
        Blank
    else if thisLbl == otherLbl  then
        OtherwiseIncorrect
    else
        Correct

updateLabel : String -> String -> LblData ->  LblData
updateLabel thisLbl otherLbl labelData =
    {labelData | str = thisLbl, state = labelState thisLbl otherLbl}


isLambdaInOfBounds el = (el > 0)

updateLambdaData : String -> NumericData Float -> NumericData Float 
updateLambdaData = updateNumeric String.toFloat isLambdaInOfBounds

isNInOfBounds n = n > 0

nEntryState = numericEntryState String.toInt isNInOfBounds

updateNData : String -> NumericData Int -> NumericData Int 
updateNData = updateNumeric String.toInt isNInOfBounds


updateN : String -> Model -> Model
updateN input model =
  {model | nData = model.nData |> updateNData input }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePulldown state ->
        ({model | pulldown = state }
        , Cmd.none
        )

    UseTotal ->
        ({model | statistic = Total}
        , Cmd.none
        )

    UseMean ->
        ({model | statistic = Mean}
        , Cmd.none
        )
    ChangeEventLbl lbl ->
        ( {model | eventLbl = model.eventLbl |> updateLabel lbl model.intervalLbl.str}
        , Cmd.none
        )

    ChangeIntervalLbl lbl ->
        ( {model | intervalLbl= model.intervalLbl |> updateLabel lbl model.eventLbl.str}
        , Cmd.none
        )

    ChangeLambda text ->
        ( {model | lambdaData = model.lambdaData |> updateLambdaData text}
        , Cmd.none
        )

    ChangeN text ->
        ( model
            |> updateN text
        , Cmd.none
        )


-- subscription
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.pulldown ChangePulldown ]

validEntry state =
    case state of
        Correct ->
            Form.validFeedback [] []

        Blank ->
            Form.validFeedback [] []

        _ ->
            Form.invalidFeedback [] [ text "Something not quite right." ]


statPulldownText model =
  case model.statistic of
    NotSelected ->
      "Select"

    Total ->
      "Total"

    Mean ->
      "Mean" 



inputFeedback model =
      [Input.success]

pulldownOutline model =
  case model.statistic of
    NotSelected ->
        Button.outlinePrimary
    
    _ ->
        Button.outlineSecondary

statPulldown model =
    InputGroup.config
        ( InputGroup.text ([ Input.placeholder (statPulldownText model), Input.disabled True] ++ inputFeedback model) )
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text "Statistic"] ]
        |> InputGroup.successors
            [InputGroup.dropdown
                model.pulldown
                { options = []
                , toggleMsg = ChangePulldown
                , toggleButton =
                    Dropdown.toggle [ pulldownOutline model, Button.small ] []
                , items =
                    [ Dropdown.buttonItem [ onClick UseTotal ] [ Html.text "Total" ]
                    , Dropdown.buttonItem [ onClick UseMean ] [ Html.text "Mean" ]
                    ]
                }
            ]
        |> InputGroup.view

singleObservationLayout event interval el n stat labelErr pErr nErr =
  Form.form []
    [ h4 [] [ Html.text "Simulation Setup"]
    , Html.br [] []
    , Form.group []
        [
          Grid.row []
            [ Grid.col  [ Col.xs7 ]
                        [ event ]
            , Grid.col [ Col.xs5 ]
                      [ el ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs7 ]
                [ interval ]
            , Grid.col [ Col.xs5 ]
                [ n ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs7 ]
                [ stat ]
            , Grid.col [ Col.xs5 ]
                [ ]
            ]
        ]
    , labelErr 
    , pErr 
    , nErr 
    ]

-- view entry point for main app
singleObservationView model =
    singleObservationLayout
       (eventEntry ChangeEventLbl model.eventLbl.state)
       (intervalEntry ChangeIntervalLbl model.intervalLbl.state)
       (lambdaEntry ChangeLambda model.lambdaData.state)
       (nEntry ChangeN model.nData.state)
       (statPulldown model)
       (labelError model)
       (lambdaError model)
       (nError model)


-- view for debug

exampleSingleObservationView =
  let
    state = 
      { eventLbl = "Phone Call"
      , intervalLbl = "Hour"
      , el = 10
      , n = 20
      , stat = "Total"
      }
  in
    singleObservationLayout
        (Html.text ("Success: " ++ state.eventLbl))
        (Html.text ("Failure: " ++ state.intervalLbl))
        (Html.text ("lambda: " ++ (String.fromFloat state.el)))
        (Html.text ("n: " ++ (String.fromFloat state.n)))
        (Html.text ("Stat: " ++ state.stat))
        (Html.text "")
        (Html.text "")
        (Html.text "")


debugView model =
    div [] 
            [ model.eventLbl |> Debug.toString |> makeHtmlText "Success: "
            , Html.br [][]
            , model.intervalLbl |> Debug.toString |> makeHtmlText "Failure: "
            , Html.br [][]
            , model.lambdaData |> Debug.toString |> makeHtmlText "lambdaData: "
            , Html.br [][]
            , model.nData |> Debug.toString |> makeHtmlText "nData: "
            , Html.br [][]
            , model.pulldown |> Debug.toString |> makeHtmlText "pulldown: "
            , Html.br [][]
            , model.statistic |> Debug.toString |> makeHtmlText "statistic: "
            , Html.br [][]
            ]

-- main view for subpage debug

view : Model -> Html Msg
view model =
    mainGrid (singleObservationView model) (debugView model) blankPvalue blankSpinner blankSpinButton blankSample blankDistPlot Hidden
