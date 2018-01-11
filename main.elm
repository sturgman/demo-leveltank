import Html exposing (Html, input, text, div, program,h4,span,button)
import Html.Lazy exposing (lazy)
import Html.Attributes as A exposing (type_,value,min,max,step) 
import Html.Events exposing (on, targetValue, onInput,onMouseEnter,onMouseLeave,onFocus,onClick)
import Sliders exposing (..)
import String as S
import Time
import Solvers exposing (euler)
import Parameters exposing (..)
import BoundedDeque as BD
import Plot as P
import MyPlot as MP
import PhysicalModels as PModel
import Controllers as C exposing (initcontroller)
import Round
-- import FontAwesome.Web as Icon
-- Model

          
--}

type Status = Idle
            | Going

type alias Model =
    { currentvalue : Float
    , currenttime : Float
    , inletflow : Float
    , simvalues : BD.BoundedDeque (Float,Float)
    , timeinterval : Float
    , math : Float -> Float -> Float
    , solver : Float -> Float -> Float
    , timestep : Float
    , steps : Int
    , status : Status
    , controller : C.PIDBasic
    , outletflow : List Float
    , transgain : Float
    , iae : Float
    }

 

miny = 0.0
maxy = 5.0
               
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

mycontroller = { initcontroller |
                     setPoint = 2.5
               }

init : (Model, Cmd Msg)
init =
    let
        result = 2.5
        math = PModel.mytank 10.0
        mysolver = euler timestep math
    in 
    ({ currentvalue = result
     , currenttime = 0.0
     , inletflow = 10.0
     , simvalues = (BD.fromList dequesize [(0.0,result)])
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

renderbuttonsgen msg txt icon =
    button
    [ onClick msg, A.class "btn" ]
    [ (Html.i
           [A.class icon]
           [])
    , text txt
    ]

renderbuttons msg txt = 
    button
    [ onClick msg ]
    [ text txt
    ]


rendercontroller model =
    div []
        [ Html.map UpdtCont (lazy C.viewPID model.controller)
        ]
{-            
renderinputs model =
    div []
        [ Html.map Updt1 (lazy sliderView model.p)
        ]
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
            

-- renderhistory model =
--     div []
--         (BD.toList (BD.map renderpoint model.simvalues))


        
    
renderhistory model =
    MP.fixedRangePlot
        P.defaultSeriesPlotCustomizations minx maxx miny maxy
        [ MP.myseries (Tuple.first >> (List.map (\(x,y) -> MP.smallcircle x y))) -- the argument to myseries and p.line are functions that extract the right series from the data.
        , P.line (Tuple.second >> (List.map (\(x,y) -> P.clear x y)))
        ]
        ((BD.toList model.simvalues),actdata)
    

renderpoint value =
    Html.p [] [text (toString value)]

                
convertoint param =
    case String.toInt param of
        Err msg -> 0
        Ok num -> num


-- Update
                  
type Msg = SimTime Time.Time
         | EqTime Time.Time
         | ToggleState
         | UpdtCont C.Msg
         | ResetSim


simoreq model t =
    if model.steps % steplonginterval == 0 then
        SimTime t
    else
        EqTime t
    
           
update msg model =
    case msg of

        SimTime newtime ->
            let
                lasttime = model.currenttime
                lastvalue = model.currentvalue
                currenttime = lasttime + model.timestep
                lastinflow = model.inletflow
                controller = (C.update
                                  timestep
                                  model.controller
                                  (model.currentvalue*model.transgain)
                             )
                outflow = Maybe.withDefault 10.0 (List.head model.outletflow)
                outletflow = Maybe.withDefault [10.0] (List.tail model.outletflow)
                newinletflow = ( euler
                                 model.timestep
                                 (PModel.mypump controller.output)
                                 lasttime
                                 lastinflow
                               ) 
                math = PModel.tank 20.0 newinletflow outflow
                solver = euler timestep math
                steps = model.steps + 1
                currentvalue = (clamp
                                    0.0
                                    5.0
                                    (solver lasttime lastvalue))
                error = abs (currentvalue -
                             model.controller.setPoint/model.transgain) *
                        model.timestep
            in
                ({ model |
                       currentvalue = currentvalue,
                       currenttime = currenttime,
                       inletflow = newinletflow,
                       simvalues = model.simvalues |>
                                   BD.pushBack (lasttime,lastvalue),
                       steps = steps,
                       controller = controller,
                       outletflow = outletflow,
                       iae = model.iae - error
                       
                 }, Cmd.none)

        EqTime newtime ->
            let
                lasttime = model.currenttime
                lastvalue = model.currentvalue
                currenttime = lasttime + model.timestep
                lastinflow = model.inletflow
                controller = (C.update
                                  timestep
                                  model.controller
                                  (model.currentvalue*model.transgain)
                             )
                newinletflow = ( euler
                                 model.timestep
                                 (PModel.mypump controller.output)
                                 lasttime
                                 lastinflow
                               ) 
                outflow = Maybe.withDefault 10.0 (List.head model.outletflow)
                outletflow = Maybe.withDefault [10.0] (List.tail model.outletflow)
                math = PModel.tank 20.0 newinletflow outflow
                solver = euler timestep math
                steps = model.steps + 1
                currentvalue = (clamp
                                    0.0
                                    5.0
                                    (solver lasttime lastvalue))
                error = abs (currentvalue -
                             model.controller.setPoint/model.transgain)
                        * model.timestep
            in ({ model |
                      currentvalue = currentvalue,
                      currenttime = currenttime,
                      inletflow = newinletflow,
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
        Time.every (Time.second * timestep) (simoreq model)

