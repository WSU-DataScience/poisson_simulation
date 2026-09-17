module SquareHistogram exposing (..)

-- Implements that square histogram for generating a discrete distribution
-- See Fast Generation of Discrete Random Variables (2004) Marsaglia et al.

import List.Extra exposing (..)
import Array exposing (..)

-- Components of a square histogram
type alias Bar = { i : Int
                 , pi : Float
                 , k : Int
                 , v : Float
                 }


type alias SquareHistogram = List Bar


initBar a i p = 
     { i = i
     , pi = p
     , k = i
     , v = (toFloat (i + 1)) * a
     }


initSqrHist : List Float -> SquareHistogram
initSqrHist ps =
    let
        n = List.length ps
        a = 1.0 / (toFloat n)
        ks = List.range 0 n
    in
        List.map2 (initBar a) ks ps

        
type alias SortedBars = { n : Int
                        , a : Float
                        , under : List Bar
                        , over : List Bar
                        , full : List Bar
                        }

        

emptySortedBars : Int -> SortedBars
emptySortedBars n = { n = n
                    , a = 1.0 / (toFloat n)
                    , under = []
                    , over = []
                    , full = []
                    }


processBar : Bar -> SortedBars -> SortedBars
processBar bar sortedBars =
    let
        fillHeight = sortedBars.a
    in
        if bar.pi < fillHeight then
            { sortedBars | under = bar :: sortedBars.under }
        else if bar.pi > fillHeight then
            { sortedBars | over = bar :: sortedBars.over }
        else
            { sortedBars | full = bar :: sortedBars.full }


sortBars : List Bar -> SortedBars
sortBars bars = 
    let
        n = List.length bars
    in 
        List.foldl processBar (emptySortedBars n) bars


updateBars : SortedBars -> SortedBars
updateBars bars =
    let
        small = bars.under |> List.sortBy .pi 
        big = bars.over  |> List.sortBy .pi |> List.reverse
    in
        case (small, big) of
            ([], _) ->
                bars

            (_, []) ->
                bars

            (minBar :: restUnder, maxBar :: restOver) ->
                let
                    min = { minBar | k = maxBar.i
                                   , v = (toFloat minBar.i) * bars.a + minBar.pi
                                   , pi = bars.a
                          } 
                    max = { maxBar | pi = maxBar.pi - (bars.a - minBar.pi) } 
                    full = min :: bars.full
                in
                    if max.pi < bars.a then 
                        updateBars { bars | under = max :: restUnder
                                   , over = restOver
                                   , full = full
                                   }
                    else if max.pi > bars.a then
                        updateBars { bars | under = restUnder
                                   , over = max :: restOver
                                   , full = full
                                   }
                    else
                        updateBars { bars | under = restUnder
                                   , over = restOver
                                   , full = max :: full
                                   }


postProcBars bars =
    (bars.under ++ bars.over ++ bars.full) |> List.sortBy .i


convertToSquareHistogram : Float -> Array Float -> Array Int -> Int -> Float -> Int
convertToSquareHistogram n vs ks min u =
    let
        j = u * n |> floor
        mv = vs |> Array.get j
        mk = ks |> Array.get j
    in
        case (mv, mk) of
            (Nothing, _) ->
                -1

            (_, Nothing) ->
                -1

            (Just v, Just k) ->
                if u < v then min + j else min + k


makeConvertToSquareHistogram : Int -> List Bar -> (Float -> Int)
makeConvertToSquareHistogram min bars =
    let
        n = bars |> List.length |> toFloat
        vs = bars |> List.map .v |> Array.fromList
        ks = bars |> List.map .k |> Array.fromList
    in
        convertToSquareHistogram n vs ks min
