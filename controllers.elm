module Controllers exposing (..)

-- PID
-- A controller with the ability to apply
-- P, I, and/or D components to control action
-- uses the following equation:
-- m(t) = bias + kp*e + ki*integral(e) + kd*derivative(e)

import Html exposing (div, h4, label, text, input)
import Html.Events exposing (on, targetValue, onInput,onClick)
import Html.Attributes as A exposing (type_,value,min,max,step) 
import Html.Lazy exposing (lazy)
import Result exposing (..)
import Sliders exposing (..)
import String as S
import Json.Decode as Json


type alias PIDBasic =
    { kp : Float
    , ki : Float
    , kd : Float
    , errorintegral : Float
    , lastE : Float
    , setPoint : Float
    , output : Float
    , mode : Mode
    , manualOutput : SliderModel
    }


type Mode
    = Auto
    | Manual


update dt controller feedback =
    case controller.mode of
        Auto ->
            let
                c = controller

                error =
                    c.setPoint - feedback

                delta_error =
                    error - c.lastE

                pterm =
                    c.kp * error

                iterm =
                    c.iterm + error * dt

                dterm =
                    c.kd * delta_error / dt

                output =
                    pterm + iterm * c.ki + dterm
            in
            { controller
                | errorintegral = iterm
                , lastE = error
                , output = output
            }

        Manual ->
            controller


updatecontroller : Msg -> PIDBasic -> PIDBasic
updatecontroller msg model =
    case msg of
        Updtkp num ->
            let
                val =
                    withDefault model.kp (S.toFloat num)
            in
            { model | kp = val }

        Updtki num ->
            let
                val =
                    withDefault model.ki (S.toFloat num)
            in
            { model | ki = val }

        Updtkd num ->
            let
                val =
                    withDefault model.kd (S.toFloat num)
            in
            { model | kd = val }

        ToggleMode ->
            { model
                | mode = notMode model.mode
            }

        UpdtM sldmsg ->
            let
                num = (extractvalue sldmsg model.manualOutput)
            in 
            { model |
                  manualOutput = sliderUpdate sldmsg model.manualOutput ,
                  output = num
            }


notMode mode =
    case mode of
        Manual ->
            Auto

        Auto ->
            Manual


initcontroller =
    (PIDBasic
         -- kp
         0.2
         -- ki
         0.0
         -- kd
         0.0
         -- errorintegral
         0.0
         -- lastE
         0.0
         -- setPoint
         0.0
         -- output
         0.0
         -- mode
         Manual
         -- slider
         (SliderModel
              50.0
              0.0
              100.0
              "0.01"
              "Controller Output"
         )
    )


type Msg
    = Updtkp String
    | Updtki String
    | Updtkd String
    | ToggleMode
    | UpdtM SliderMsg


viewPID cont =
    let
        currentMode =
            case cont.mode of
                Auto -> "On Auto Mode"
                Manual -> "On Manual Mode"

        renderInput =
            case cont.mode of
                Auto ->
                    [ renderNumberInput Updtkp "kp" cont.kp
                    , renderNumberInput Updtki "ki" cont.ki
                    , renderNumberInput Updtkd "kd" cont.kd
                    ]
                Manual ->
                    [Html.map UpdtM (lazy sliderView cont.manualOutput)]
                    
    in 
    div []
        [ text "Controller"
        , div []
              [ label []
                    [ input [ type_ "checkbox", onClick ToggleMode ] []
                    , text currentMode
                    ]
              ]
        , div []
            renderInput
        ]


renderNumberInput msg lbl v =
    let
        style = A.style
                [ ("width","15%")
                ]
    in 
    label []
        [ text lbl
        , input
              [ style
              , type_ "number"
              , onchange msg
              , value <| toString <| v 
              ]
              []
        ]


onchange tagger =
    on "change" (Json.map tagger targetValue)
