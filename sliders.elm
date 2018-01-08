module Sliders exposing (..)

import Html exposing (Html, input, text, div,h4,span)
import Html.Lazy exposing (lazy)
import Html.Attributes as A exposing (type_,value,min,max,step) 
import Html.Events exposing (on, targetValue, onInput,onMouseEnter,onMouseLeave,onFocus)
import Json.Decode as Json
import String as S
import List

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
        renderMyNumber = renderNumberInput style model.max model.min model.step
    in 
    div []
        [ text (model.label ++ ":")
        , div []
            [ input (List.append style
                         [ type_ "range"
                         , onInput Slide
                         , A.max <| toString <| model.max
                         , A.min <| toString <| model.min
                         , step model.step
                         , value <| toString <| model.value
                         ]
                    ) []
                    
            , span []
                -- The lazy here necessary to prevent continuous update as the model integrates.
                [ lazy renderMyNumber model.value
                ] 
            ]
       ]
            
renderNumberInput style max min step v =
    input (List.append style
               [type_ "number"
               , onchange TextIn
               , A.max <| toString <| max
               , A.min <| toString <| min
               , A.step step
               , value <| toString <| v
               ]
          ) []

type SliderMsg = Slide String
               | TextIn String

        
sliderUpdate : SliderMsg -> SliderModel -> SliderModel
sliderUpdate msg model =
    case msg of
        Slide num ->
            case (S.toFloat num) of
                Ok v ->
                    if v < model.min then
                        model
                            
                    else if v > model.max then
                        model

                    else
                        { model | value = v}
                            
                Err v ->
                    model
                
        TextIn num ->
            case (S.toFloat num) of
                Ok v ->
                    if v < model.min then
                        model
                            
                    else if v > model.max then
                        model

                    else
                        { model | value = v}

                Err v ->
                    model

onchange tagger = on "change" (Json.map tagger targetValue)

extractvalue : SliderMsg -> SliderModel -> Float
extractvalue msg model=
    case msg of
        Slide num ->
            case (S.toFloat num) of
                Ok v ->
                    if v < model.min then
                        model.value
                            
                    else if v > model.max then
                        model.value

                    else
                        v
                            
                Err errmsg ->
                    model.value
                
        TextIn num ->
            case (S.toFloat num) of
                Ok v ->
                    if v < model.min then
                        model.value
                            
                    else if v > model.max then
                        model.value

                    else
                        v

                Err errmsg ->
                    model.value
    
