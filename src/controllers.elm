module Controllers exposing (Mode(..), Msg(..), PIDBasic, initcontroller, notMode, onchange, renderNumberInput, renderkd, renderki, renderkp, update, updatecontroller, viewPID)

{- PID A controller with the ability to apply P, I, and/or D
   components to control action uses the following equation: $m(t) = bias
   + kp*e + ki*integral(e) + kd*derivative(e)$
-}

import Html exposing (div, h4, input, label, text,button)
import Html.Attributes as A exposing (max, min, step, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Maybe exposing (..)
import Sliders exposing (..)
import String as S


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



{- Updates the value of the controller output. Note that this is not
   the TEA update.
-}


update : Float -> PIDBasic -> Float -> PIDBasic
update dt controller feedback =
    case controller.mode of
        Auto ->
            let
                c =
                    controller

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

                slider =
                    controller.manualOutput

                manualOutput =
                    { slider
                        | value = clamp 0 100 output
                    }
            in
            { controller
                | errorintegral = iterm
                , lastE = error
                , output = clamp 0 100 output
                , manualOutput = manualOutput
            }

        Manual ->
            controller



{- This is the TEA update for a PID controller. -}


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
                num =
                    extractvalue sldmsg model.manualOutput
            in
            { model
                | manualOutput = sliderUpdate sldmsg model.manualOutput
                , output = num
            }


notMode mode =
    case mode of
        Manual ->
            Auto

        Auto ->
            Manual



{- Creates a default controller. Note that all units are controller
   units (%transmitter output and %controller output).
-}


initcontroller =
    PIDBasic
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
                Auto ->
                    " Auto"

                Manual ->
                    " Manual"

        otherMode =
            case cont.mode of
                Auto ->
                    "Manual"

                Manual ->
                    "Auto"
                        
        renderInput =
            case cont.mode of
                Auto ->
                    [ lazy renderkp cont.kp
                    , lazy renderki cont.ki
                    , lazy renderkd cont.kd
                    , Html.map UpdtM
                        (lazy
                            (sliderView
                                [ A.disabled True
                                ]
                            )
                            cont.manualOutput
                        )
                    ]

                Manual ->
                    [ Html.map UpdtM
                        (lazy
                            (sliderView
                                [ A.disabled False
                                , A.class "numoutput"
                                ]
                            )
                            cont.manualOutput
                        )
                    ]
    in
    div []
        [ div []
            [ label []
                [ text (S.append "Controller mode is" currentMode)
 --               , input [ type_ "checkbox", onClick ToggleMode ] []
                , renderbuttonsgen ToggleMode otherMode "automan"
                ]
            ]
        , div []
            renderInput
        ]


renderNumberInput msg lbl v =
    let
        style =
            A.style "width" "15%"
                
    in
    label []
        [ text lbl
        , input
            [ style
            , type_ "number"
            , onchange msg
            , step "0.1"
            , value <| S.fromFloat <| v
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


renderbuttonsgen msg txt icon =
    button
        [ onClick msg, A.class "btn switchmode" ]
        [ Html.i
            [ A.class icon ]
            []
        , text txt
        ]
