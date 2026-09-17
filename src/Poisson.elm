module Poisson exposing (..)

import List.Extra exposing (..)
import Array exposing (..)
import Defaults exposing (defaults)
import SquareHistogram exposing (..)

-- For debug
import DataEntry exposing (..)
import Layout exposing (..)
import SingleObservation exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col

{-
All the functions (except the last Gen function)
generate 
-}
meanPoisson : Float -> Float
meanPoisson el = el

sdPoisson : Float -> Float
sdPoisson el = el^0.5

totalLambda : Int -> Float -> Float
totalLambda n lambda = (toFloat n)*lambda

meanTotalPoisson : Int -> Float -> Float
meanTotalPoisson n lambda =
    totalLambda n lambda
    |> meanPoisson

sdTotalPoisson : Int -> Float -> Float
sdTotalPoisson n lambda =
    totalLambda n lambda
    |> sdPoisson

-- Log difference between p(k+1) and p(k)
poissonLogDiff: Float -> Int -> Float
poissonLogDiff el k = 
    if k == 0 then -1.0*el else logBase e el - logBase e (toFloat k)

-- Compute the previous log coef at X = k - 1
poissonLogCoef : Float -> Int -> Float
poissonLogCoef el k =
    let
        ks = List.range 0 k
        terms = List.map (poissonLogDiff el) ks
    in
        List.sum terms


poissonLogCoefRange : Float -> Int -> Int -> List(Float)
poissonLogCoefRange el start stop =
    let
        coefStart = poissonLogCoef el start
        nums = List.range (start + 1) stop
        logDiffs = List.map (poissonLogDiff el) nums 
    in
        (List.Extra.scanl (+) coefStart logDiffs)

poissonProbRange: Float -> Int -> Int -> List(Float)
poissonProbRange el start stop =
    let
        xs = List.map toFloat (List.range start stop)
        logProbs = poissonLogCoefRange el start stop
    in
        List.map (\logp -> e^logp) logProbs


roundFloat : Int -> Float -> Float
roundFloat digits n =
   let
     div = toFloat 10^(toFloat digits)
     shifted = n*div
   in
     round shifted |> toFloat |> \x -> x/div


trimmedXRange el =
    let
        mean = meanPoisson el
        sd = sdPoisson el
        -- Enforce the lower bound
        minX = Basics.max 0 (round (mean - 6*sd))
        -- Poisson is unbounded --> Always trim upper bound
        maxX = round (mean + 6*sd)
    in
        (minX, maxX)


trimmedXs el =
    let
        (min, max) = trimmedXRange el
    in
        List.range min max


trimmedProbs : Float -> List Float
trimmedProbs el =
    let
        (minX, maxX) = trimmedXRange el
    in
        case (minX, maxX) of
            (0, 0) -> [1.0]

            (0, n) -> 
                let
                    ps = poissonProbRange el minX (maxX - 1)
                    comp = 1.0 - (List.sum ps)
                in
                    ps ++ [comp]

            (i, j) ->
                let
                    start = List.sum (poissonProbRange el 0 minX)
                    ps = poissonProbRange el (minX + 1) (maxX - 1)
                    comp = 1.0 - (List.sum ps) - start
                in
                    [start] ++ ps ++ [comp]


twoTailLimits : Float -> Float -> (Float, Float)
twoTailLimits mean value =
    let
        diff = if (value <= mean) then mean - value else value - mean
    in
        (mean - diff, mean + diff)

makeBars el =
    let
        (min, _) = trimmedXRange el
        ps = trimmedProbs el
    in
        ps
         |> initSqrHist
         |> sortBars
         |> updateBars
         |> postProcBars


-- Used to generate individual Poisson draws
getPoissonGen : Float -> (Float -> Int)
getPoissonGen el =
    let
        (min, _) = trimmedXRange el
        ps = trimmedProbs el
        bars = ps
                |> initSqrHist
                |> sortBars
                |> updateBars
                |> postProcBars
    in
        makeConvertToSquareHistogram min bars

-- Generates that sample total of Poisson sample
getTotalPoissonGen : Int -> Float -> (Float -> Int)
getTotalPoissonGen n lambda =
    let
        el = (toFloat n)*lambda
    in
        getPoissonGen el

{- 





This pages main for debugging etc.





-}

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
                   , totalLambdaData : Maybe Float
                   , xs : List Int
                   , logPs : List Float
                   , ps : List Float
                   }

-- Initialize


initModel = { eventLbl = initLbl
            , intervalLbl = initLbl
            , lambdaData = initFloat
            , nData = initInt
            , totalLambdaData = Nothing
            , xs = []
            , logPs = []
            , ps = []
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- Messages

type Msg  = ChangeEventLbl String
          | ChangeIntervalLbl String
          | ChangeLambda String
          | ChangeN String


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

updateLambda : String -> Model -> Model
updateLambda text model =
    {model | lambdaData = model.lambdaData |> updateLambdaData text}
    

isNInOfBounds n = n > 0

nEntryState = numericEntryState String.toInt isNInOfBounds

updateNData : String -> NumericData Int -> NumericData Int 
updateNData = updateNumeric String.toInt isNInOfBounds

updateN : String -> Model -> Model
updateN input model =
  {model | nData = model.nData |> updateNData input }

updateTotalLambda : Model -> Model
updateTotalLambda model =
    case (model.nData.val, model.lambdaData.val) of
        (Just n, Just el) ->
            {model | totalLambdaData = Just ((toFloat n)*el)}
        
        _ ->
            {model | totalLambdaData = Nothing}

updateXs : Model -> Model
updateXs model =
    case (model.lambdaData.val, model.lambdaData.state) of
        (Just el, Correct) ->
            {model | xs = trimmedXs el}

        _ -> 
            {model | xs = []}

updatePs : Model -> Model
updatePs model =
    case (model.lambdaData.val, model.lambdaData.state) of
        (Just el, Correct) ->
            {model | ps = trimmedProbs el}

        _ -> 
            {model | ps = []}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeEventLbl lbl ->
        ( {model | eventLbl = model.eventLbl |> updateLabel lbl model.intervalLbl.str}
        , Cmd.none
        )

    ChangeIntervalLbl lbl ->
        ( {model | intervalLbl= model.intervalLbl |> updateLabel lbl model.eventLbl.str}
        , Cmd.none
        )

    ChangeLambda text ->
        ( model 
          |> (updateLambda text)
          |> updateTotalLambda
          |> updateXs
          |> updatePs
        , Cmd.none
        )

    ChangeN text ->
        ( model
            |> updateN text
            |> updateTotalLambda
        , Cmd.none
        )


-- subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


validEntry state =
    case state of
        Correct ->
            Form.validFeedback [] []

        Blank ->
            Form.validFeedback [] []

        _ ->
            Form.invalidFeedback [] [ text "Something not quite right." ]

inputFeedback model =
      [Input.success]


singleObservationLayout event interval p n labelErr pErr nErr =
  Form.form []
    [ h4 [] [ Html.text "Simulation Setup"]
    , Html.br [] []
    , Form.group []
        [
          Grid.row []
            [ Grid.col  [ Col.xs7 ]
                        [ event ]
            , Grid.col [ Col.xs5 ]
                      [ p ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs7 ]
                [ interval ]
            , Grid.col [ Col.xs5 ]
                [ n ]
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
       (labelError model)
       (lambdaError model)
       (nError model)


-- view for debug

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
            , model.totalLambdaData |> Debug.toString |> makeHtmlText "totalLambdaData: "
            , Html.br [][]
            , model.xs |> Debug.toString |> makeHtmlText "xs: "
            , Html.br [][]
            , model.logPs |> Debug.toString |> makeHtmlText "log(ps): "
            , Html.br [][]
            , model.ps |> List.map (roundFloat 6) |> Debug.toString |> makeHtmlText "ps: "
            , Html.br [][]
            , model.ps |> List.sum |> Debug.toString |> makeHtmlText "sum(ps): "
            , Html.br [][]
            , model.ps |> List.sum |> Debug.toString |> makeHtmlText "sum(ps): "
            , Html.br [][]
            , model.lambdaData |> .val |> Maybe.map makeBars |> Debug.toString |> makeHtmlText "Bars: "
            , Html.br [][]
            , model.lambdaData |> .val |> Maybe.map makeBars |> Debug.toString |> makeHtmlText "Bars: "
            , Html.br [][]
            , model.lambdaData |> .val |> Maybe.map getPoissonGen |> Debug.toString |> makeHtmlText "Gen: "
            , Html.br [][]
            ]

-- main view for subpage debug

view : Model -> Html Msg
view model =
    mainGrid (singleObservationView model) (debugView model) blankPvalue blankSpinner blankSpinButton blankSample blankDistPlot Hidden