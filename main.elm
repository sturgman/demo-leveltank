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
-- import FontAwesome.Web as Icon
-- Model

-- Example data
expdata t = 
    e^(-0.05*t)

actydata = List.map (toFloat >> expdata) (List.range 0 240)
actdata = List.map2 (\a b -> (a,b)) (List.map toFloat (List.range 0 240)) actydata
          
--}

type Status = Idle
            | Going

type alias Model =
    { p : SliderModel
    , currentvalue : Float
    , currenttime : Float
    , simvalues : BD.BoundedDeque (Float,Float)
    , timeinterval : Float
    , math : Float -> Float -> Float
    , solver : Float -> Float -> Float
    , timestep : Float
    , steps : Int
    , status : Status
    }

mathmodel : Float -> Float -> Float -> Float
mathmodel = PModel.simple

            
            
minx = 0.0
maxx = (toFloat dequesize) * timeinterval

miny = 0.0
maxy = 1
               
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : (Model, Cmd Msg)
init =
    let
        val1 = -0.05
        result = 1
        defmin = -1.0
        defmax = 1.0
        math = mathmodel val1
        mysolver = euler timestep math
    in 
    ({ p =
           (SliderModel
                val1
                defmin
                defmax
                "0.01"
                "Param1"
           )
     , currentvalue = result
     , currenttime = 0.0
     , simvalues = (BD.fromList dequesize [(0.0,result)])
     , timeinterval = timeinterval
     , timestep = timestep
     , steps = 0
     , math = math
     , solver = mysolver
     , status = Idle
     }
    , Cmd.none)

-- View
    
view model =
    div []
        [ renderbuttons model "play-circle"
        , renderinputs model
        , renderresults model
        , renderhistory model
        ]

renderbuttons model icon=
    let
        buttontext =
            case model.status of
                Idle -> "Start"
                Going -> "Pause"
    in 
        div []
            [ button
                  [ onClick ToggleState ]
                  [ Html.i
                        [A.class ("far fa-" ++ icon) ]
                        []
                  ]
            ]

renderinputs model =
    div []
        [ Html.map Updt1 (lazy sliderView model.p)
        ]

        
renderresults model =
    let
        p = model.p.value
    in
        div []
            [ h4 [] [text (toString (model.currentvalue)) ]]


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
                  
type Msg = Updt1 SliderMsg
         | SimTime Time.Time
         | EqTime Time.Time
         | ToggleState

simoreq model t =
    if model.steps % steplonginterval == 0 then
        SimTime t
    else
        EqTime t
    
           
update msg model =
    case msg of
        Updt1 v ->
            let
                num = (extractvalue v model.p)
                math = mathmodel num
            in
                ({ model |
                       p = sliderUpdate v model.p,
                       math = math,
                       solver = euler timestep math             
                 }, Cmd.none)

        SimTime newtime ->
            let
                lasttime = model.currenttime
                lastvalue = model.currentvalue
                currenttime = lasttime + model.timestep
                solver = model.solver
                steps = model.steps + 1
            in
                ({ model |
                       currentvalue = solver lasttime lastvalue,
                       currenttime = currenttime,
                       simvalues = model.simvalues |>
                                   BD.pushBack (lasttime,lastvalue),
                       steps = steps
                 }, Cmd.none)

        EqTime newtime ->
            let
                lasttime = model.currenttime
                currenttime = lasttime + model.timestep
                lastvalue = model.currentvalue
                solver = model.solver
                steps = model.steps + 1
            in ({ model |
                      currentvalue = solver lasttime lastvalue,
                      currenttime = currenttime,
                      steps = steps
                }, Cmd.none)

        ToggleState ->
            case model.status of
                Idle -> ({model | status = Going},Cmd.none)
                Going -> ({model | status = Idle},Cmd.none)

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
    
        

-- Subs
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == Idle || model.currenttime > maxx then
        Sub.none
    else
        Time.every (Time.second * 0.1) (simoreq model)

