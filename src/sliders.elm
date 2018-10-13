module Sliders exposing (SliderModel, SliderMsg(..), extractvalue, onchange, renderNumberInput, sliderUpdate, sliderView)

{- Module implementing a syncronized slider plus number input box.  In
   the app it is used as the input method to specify the controller
   output. It also provides feedback to the user about what the automatic
   controller is doing.
-}

import Html exposing (Html, div, h4, input, span, text)
import Html.Attributes as A exposing (max, min, step, type_, value)
import Html.Events exposing (on, onFocus, onInput, onMouseEnter, onMouseLeave, targetValue)
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import List
import Round
import String


-- Sliders


type alias SliderModel =
    { value : Float
    , min : Float
    , max : Float
    , step : String
    , label : String
    }



--sliderView : SliderModel -> Html SliderMsg


sliderView style model =
    let
        renderMyNumber =
            renderNumberInput style model.max model.min model.step
    in
    div []
        [ text (model.label ++ ":")
        , div []
            [ input
                (List.append style
                    [ type_ "range"
                    , onchange Updt
                    , A.max <| String.fromFloat <| model.max
                    , A.min <| String.fromFloat <| model.min
                    , step model.step
                    , value <| String.fromFloat <| model.value
                    ]
                )
                []
            , span []
                -- The lazy here necessary to prevent continuous
                -- rendering of the textbox as the model integrates.
                [ lazy renderMyNumber model.value
                ]
            ]
        ]


renderNumberInput style max min step v =
    input
        (List.append style
            [ type_ "number"
            , onchange Updt
            , A.max <| String.fromFloat <| max
            , A.min <| String.fromFloat <| min
            , A.step step
            , value <| Round.round 2 v
            ]
        )
        []


type SliderMsg
    = Updt String


sliderUpdate : SliderMsg -> SliderModel -> SliderModel
sliderUpdate msg model =
    case msg of
        Updt num ->
            case String.toFloat num of
                Just v ->
                    if v < model.min then
                        model

                    else if v > model.max then
                        model

                    else
                        { model | value = v }

                Nothing ->
                    model


onchange tagger =
    on "change" (Json.map tagger targetValue)



{- Helper function to extract the value from a slider into a float. -}


extractvalue : SliderMsg -> SliderModel -> Float
extractvalue msg model =
    case msg of
        Updt num ->
            case String.toFloat num of
                Just v ->
                    if v < model.min then
                        model.value

                    else if v > model.max then
                        model.value

                    else
                        v

                Nothing ->
                    model.value
