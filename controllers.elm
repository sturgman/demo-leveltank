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
    , bias : Float
    }


type Mode
    = Auto
    | Manual

update : Float -> PIDBasic -> Float -> PIDBasic
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
                    clamp -20.0 20.0 (c.errorintegral + error * dt)

                dterm =
                    c.kd * delta_error / dt

                output =
                    c.bias + pterm + iterm * c.ki + dterm

                slider = controller.manualOutput

                manualOutput = { slider |
                                     value = (clamp 0 100 output)
                               } 
            in
            { controller
                | errorintegral = iterm
                , lastE = error
                , output = (clamp 0 100 output)
                , manualOutput =  manualOutput
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
         7.2
         -- ki
         1.3
         -- kd
         2.4
         -- errorintegral
         0.0
         -- lastE
         0.0
         -- setPoint
         50.0
         -- output
         50.0
         -- mode
         Manual
         -- slider
         (SliderModel
              50.0
              0.0
              100.0
              "0.5"
              "Controller Output"
         )
         50.0
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
                Auto -> " Auto"
                Manual -> " Manual"

        renderInput =
            case cont.mode of
                Auto ->
                    [ lazy renderkp cont.kp
                    , lazy renderki cont.ki
                    , lazy renderkd cont.kd
                    , Html.map UpdtM (lazy (sliderView [A.disabled True]) cont.manualOutput)
                    ]
                Manual ->
                    [Html.map UpdtM (lazy (sliderView [A.disabled False]) cont.manualOutput)]
                    
    in 
    div []
        [ div []
              [ label []
                    [ text "Controller mode:  "
                    , input [ type_ "checkbox", onClick ToggleMode ] []
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
              , step "0.1"
              , value <| toString <| v
              ]
              []
        ]

renderkp v =
    renderNumberInput Updtkp "  kp: " v

renderki v =
    renderNumberInput Updtki "  ki: " v

renderkd v =
    renderNumberInput Updtkd "  kd: " v

        
onchange tagger =
    on "change" (Json.map tagger targetValue)
