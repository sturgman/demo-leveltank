{- An app that simulates a holding tank with one input and one output
liquid streams (think of a bath tub with the faucent on and the drain
open.  The liquid flow rate of the outlet stream is considered a
disturbance and has a semi random value given by
Paramters.outflowpattern. The inlet flow rate can be either controlled
by an automatic feedback controller or manually adjusted by the
user. The app includes a plot in which the green data points represent
the actual level of the tank, and the pink line represents the desired
set-point for the tank's level.
-}

import Html exposing (Html, input, text, div, program,h4,span,button)
import Html.Lazy exposing (lazy)
import Html.Attributes as A exposing (type_,value,min,max,step) 
import Html.Events exposing (on, targetValue, onInput,onFocus,onClick)
import String as S
import Time

import BoundedDeque as BD -- folkertdev/elm-deque
import Plot as P -- terezka/elm-plot
import Round -- myrho/elm-round

import Sliders exposing (..) -- Module for a custom input used in the app
import MyPlot as MP -- Module to customize the plot in the app
import PhysicalModels as PModel -- Module that includes physical models
import Controllers as C exposing (initcontroller) -- Module implements PID control
import Solvers exposing (euler) -- Module implements a numerical integrator
import Parameters exposing (..) -- Parameters for this specific app


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{- Represents the status of the simulation. It can either be paused
(Idle) or running (Going).
-}
type Status = Idle
            | Going

{- This is the model for the entire app. There is a lot here!

* I usee BoundedDeque for the history of the simulation. This will
hopefully facilitate automatically-rescaling plots in the future.

* Model.math is a function that takes the current time and the current
state variable (the tank level). It can be built with some of the
functions in the PhysicalModels module. It represents the mathematical
model being simulated.

* Model.solver is a function that takes the current time and the
current state-variable. It can be built from an integrator in the
module Solvers. It is able to integrate the Model.math.
-}
type alias Model =
    { currentvalue : Float -- The current value of the tank level
    , currenttime : Float -- The current simulation time
    , inletflow : Float -- The value of the inlet flow
    , simvalues : BD.BoundedDeque (Float,Float) -- History of the level of the tank
    , timeinterval : Float -- The update interval for the plot
    , math : Float -> Float -> Float -- Mathematical model being simulated (holding tank)
    , solver : Float -> Float -> Float -- The solver coupled to the mathematical model
    , timestep : Float -- The actual timestep for integration of the mathematical model
    , steps : Int -- The number of steps that have ellapsed
    , status : Status -- The status of the simulation (paused or running)
    , controller : C.PIDBasic -- The controller in charge of the inlet flow rate
    , outletflow : List Float -- A list describing the pattern of outletflows (semi-random)
    , transgain : Float -- A float representing the gain of the transmitter.
    , iae : Float -- The integral of the absolute error (related to the "score")
    }


{- Initializing the model.
-}
init : (Model, Cmd Msg)
init =
    let
        initialHeight = 2.5
        math = PModel.mytank 10.0
        mysolver = euler timestep math
    in 
    ({ currentvalue = initialHeight
     , currenttime = 0.0
     , inletflow = 10.0
     , simvalues = (BD.fromList dequesize [(0.0,initialHeight)])
     , timeinterval = timeinterval
     , timestep = timestep
     , steps = 0
     , math = math
     , solver = mysolver
     , status = Idle
     , controller = C.initcontroller
     , outletflow = outflowpattern
     , transgain = transgain
     , iae = initiae
     }
    , Cmd.none)


-- View
view model =
    let
        statusicon =
            case model.status of
                Idle -> "far fa-play-circle"
                Going -> "far fa-pause-circle"
        statustext =
            case model.status of
                Idle -> "  PLAY"
                Going -> "  PAUSE"
    in
        div []
            [ div [] [ (renderbuttonsgen
                            ToggleState
                            statustext
                            statusicon)
                     , (renderbuttonsgen
                            ResetSim
                            "  RESET"
                            "far fa-caret-square-left")]
            , rendercontroller model
            , renderresults model
            , renderhistory model
            ]

{- What follows is a bunch of helper view functions. To keep the main
view tidy.
-}
renderbuttonsgen msg txt icon =
    button
    [ onClick msg, A.class "btn" ]
    [ (Html.i
           [A.class icon]
           [])
    , text txt
    ]


rendercontroller model =
    div []
        [ Html.map UpdtCont (lazy C.viewPID model.controller)
        ]

{- This uses the integral of the absolute value of the error
to calculate a score. The student can compare the performance
of various control strategies using this score. Note that the
number is not the only criteria by which to assess a control
strategy.
-}    
renderresults model =
    let
        mystyle = A.style [ ("font-weight","bold")
                  , ("color","#ff0000")
                  ]
    in 
        div []
            [ text "Your score:  "
            , span [mystyle] [text (Round.round 1 (model.iae)) ]]
            

{- This uses the plot module to make a graph of the level vs time. The
value of the setpoint is also shown in the graph.
-}
renderhistory model =
    MP.fixedRangePlot
        P.defaultSeriesPlotCustomizations minx maxx miny maxy
        [ MP.myseries (Tuple.first >> (List.map (\(x,y) -> MP.smallcircle x y))) -- the argument to myseries and p.line are functions that extract the right series from the data.
        , P.line (Tuple.second >> (List.map (\(x,y) -> P.clear x y)))
        ]
        ((BD.toList model.simvalues),actdata)
    

-- Update
                  
type Msg = SimTime Time.Time -- Performs a timestep and conditionally updates plot
         | ToggleState -- Toggle between Idle and Going
         | UpdtCont C.Msg -- Update the controller settings
         | ResetSim -- Resets the simulation
  
           
update msg model =
    case msg of

        SimTime newtime ->
            let
                -- store the current values of the model
                lasttime = model.currenttime
                lastvalue = model.currentvalue
                lastinflow = model.inletflow

                -- Conditionally update the history:
                simvalues = 
                    if model.steps % steplonginterval == 0 then
                        model.simvalues |>
                                   BD.pushBack (lasttime,lastvalue)
                    else 
                        model.simvalues

                -- calculate the new time
                currenttime = lasttime + model.timestep

                -- Update the controller based on the current and past
                -- error. model.transgain converts the error from 
                -- physical units of length to controller units of 
                -- % trasnmitter output.
                controller = (C.update
                                  timestep
                                  model.controller
                                  (model.currentvalue*model.transgain)
                             )

                -- Process the outflow pattern to in outletflow to get
                -- the current flow rate of the outlet stream. The
                -- default value of 10.0 is the steady state value
                -- for the current process parameters.
                outflow = (Maybe.withDefault 
                               10.0 
                               (List.head model.outletflow)
                          ) 
                outletflow = (Maybe.withDefault 
                                  [10.0] 
                                  (List.tail model.outletflow)
                             ) 
                -- Calcualte the new inlet flow. In  this particular
                -- model we have a pump at the inlet. The flow rate
                -- through this pump is determined by the signal from
                -- the controller. An awkward issue with the way
                -- I am doing things is that I am specifying part of
                -- the physical model here instead of in the math
                -- function of the model.
                newinletflow = ( euler
                                 model.timestep
                                 (PModel.mypump controller.output)
                                 lasttime
                                 lastinflow
                               )

                -- Construct the current physical model
                -- and perform a timestep..
                math = PModel.tank 20.0 newinletflow outflow
                solver = euler timestep math
                steps = model.steps + 1
                currentvalue = (clamp
                                    0.0
                                    5.0
                                    (solver lasttime lastvalue))

                -- Compute the new value of the error.
                error = abs (currentvalue -
                             model.controller.setPoint/model.transgain) *
                        model.timestep
            in
                ({ model |
                       currentvalue = currentvalue,
                       currenttime = currenttime,
                       inletflow = newinletflow,
                       simvalues = simvalues,
                       steps = steps,
                       controller = controller,
                       outletflow = outletflow,
                       iae = model.iae - error
                       
                 }, Cmd.none)


        ToggleState ->
            case model.status of
                Idle -> ({model | status = Going},Cmd.none)
                Going -> ({model | status = Idle},Cmd.none)

        UpdtCont cmsg ->
            let
                controller = (C.updatecontroller cmsg model.controller)
            in
                ({ model |
                       controller = controller
                 }, Cmd.none)

        ResetSim ->
            -- init
            let
                controller = model.controller
                updtcontroller = { controller |
                                       errorintegral = 0.0
                                 }
            in
                ( { model |
                        currentvalue = 2.5 ,
                        currenttime = 0.0 ,
                        inletflow = 10.0 ,
                        simvalues = (BD.fromList dequesize [(0.0,2.5)]) ,
                        steps = 0 ,
                        status = Idle ,
                        controller = updtcontroller ,
                        outletflow = outflowpattern ,
                        iae = initiae
                  } , Cmd.none )

                

-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == Idle || model.currenttime > maxx then
        Sub.none
    else
        Time.every (Time.second * timestep) SimTime

