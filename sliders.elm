module Sliders exposing (..)

import Html exposing (Html, input, text, div,h4,span)
import Html.Lazy exposing (lazy)
import Html.Attributes as A exposing (type_,value,min,max,step) 
import Html.Events exposing (on, targetValue, onInput,onMouseEnter,onMouseLeave,onFocus)
import Json.Decode as Json
import String as S

-- Sliders                
    
type alias SliderModel =
    { value : Float
    , min : Float
    , max : Float
    , step : String
    , label : String
    }


sliderView : SliderModel -> Html SliderMsg
sliderView model =
    div []
      [ text (model.label ++ ":")
      , div []
          [ input [ type_ "range"
                  , onInput Slide
                  , value <| toString <| model.value
                  , A.max <| toString <| model.max
                  , A.min <| toString <| model.min
                  , step model.step ] []
          , span []
              [ lazy renderNumberInput (model.value,model.max,model.min,model.step)
              ] 
          ]
      ]

renderNumberInput (v,max,min,step) =
    input [type_ "number"
                     , onchange TextIn
                     , value <| toString <| v
                     , A.max <| toString <| max
                     , A.min <| toString <| min
                     , A.step step
          ] []

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
