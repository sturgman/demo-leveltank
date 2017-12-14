import Html exposing (Html, input, text, div, program,h4,span)
import Html.Attributes as A exposing (type_,value,min,max,step) 
import Html.Events exposing (on, targetValue, onInput,onMouseEnter,onMouseLeave)
import Json.Decode as Json
import String as S
import Time

import BoundedDeque as BD
import Plot as P
import MyPlot as MP

-- Model

type alias Model =
    { param1 : SliderModel
    , param2 : SliderModel
    , param3 : SliderModel
    , currentvalue : Float
    , currenttime : Float
    , simvalues : BD.BoundedDeque (Float,Float)
    , timeinterval : Float
    }

timeinterval : Float
timeinterval = 2.0

dequesize = 120

tickintervalx =
    timeinterval*dequesize/8

ticklocationsx =
    List.map
        (\x -> (toFloat x)*tickintervalx)
        (List.range 0 8)
            
minx = 0.0
maxx = dequesize * timeinterval

miny = 0.0
maxy = 50.0*100*100
               
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
        val1 = 50.0
        val2 = 75.0
        val3 = 25.0
        result = val1*val2*val3
        defmin = 0
        defmax = 100
    in 
    ({ param1 =
           (SliderModel
                val1
                defmin
                defmax
                "0.5"
                "Param1"
           )
     , param2 =
           (SliderModel
                val2
                50
                defmax
                "0.5"
                "Param2"
           )
     , param3 =
           (SliderModel
                val3
                defmin
                50
                "0.5"
                "Param3"
           )
     , currentvalue = result
     , currenttime = 0.0
     , simvalues = (BD.fromList dequesize [(0.0,result)])
     , timeinterval = timeinterval
     }
    , Cmd.none)

-- View
    
view model =
    div []
        [ renderinputs model
        , renderresults model
        , renderhistory model
        ]

renderinputs model =
    div []
        [ Html.map Updt1 (sliderView model.param1)
        , Html.map Updt2 (sliderView model.param2)
        , Html.map Updt3 (sliderView model.param3)
        ]

        
renderresults model =
    let
        param1 = model.param1.value
        param2 = model.param2.value
        param3 = model.param3.value
    in
        div []
            [ h4 [] [text (toString (param1*param2*param3)) ]]


-- renderhistory model =
--     div []
--         (BD.toList (BD.map renderpoint model.simvalues))


xaxis = MP.myaxis ticklocationsx

pltwidth = 520
pltheight = 400
        
fixedRangePlot default minX maxX minY maxY =
    P.viewSeriesCustom
        { default
            | width = pltwidth
            , height = pltheight
            , margin =
              { top  = 20
              , right = 20
              , bottom = 40
              , left = 60
              }
            , horizontalAxis = xaxis
            , toDomainLowest = always minY
            , toDomainHighest = always maxY
            , toRangeLowest = always minX
            , toRangeHighest = always maxX
            , attributes = [(A.style
                                 [ ("max-width","520px")
                                 , ("width","100%")
                                 ])
                            ]
        }     
        

renderhistory model =
    fixedRangePlot
        P.defaultSeriesPlotCustomizations minx maxx miny maxy
        [ MP.myseries (List.map (\(x,y) -> MP.smallcircle x y)) ]
        (BD.toList model.simvalues)
    

renderpoint value =
    Html.p [] [text (toString value)]

                
convertoint param =
    case String.toInt param of
        Err msg -> 0
        Ok num -> num


-- Update
                  
type Msg = Updt1 SliderMsg
         | Updt2 SliderMsg
         | Updt3 SliderMsg
         | Propagate Time.Time
    
update msg model =
    case msg of
        Updt1 v ->
            let
                num = (extractvalue v model.param1)
                result = (calculatecurrent num model.param2.value model.param3.value)
            in
                ({ model |
                       param1 = sliderUpdate v model.param1 ,
                       currentvalue = result
                 }, Cmd.none)
        Updt2 v ->
            let
                num = (extractvalue v model.param2)
                result = (calculatecurrent model.param1.value num model.param3.value)
            in
                ({ model | param2 =
                       sliderUpdate v model.param2 ,
                       currentvalue = result
                 }, Cmd.none)
        Updt3 v ->
            let
                num = (extractvalue v model.param3)
                result = (calculatecurrent model.param1.value model.param2.value num)
            in
                ({ model | param3 =
                       sliderUpdate v model.param3 ,
                       currentvalue = result
                 }, Cmd.none)
        Propagate newtime ->
            let
                lasttime = model.currenttime
                currenttime = lasttime + model.timeinterval
            in
                ({ model |
                       simvalues = model.simvalues
                                 |> BD.pushBack (currenttime,model.currentvalue) , 
                       currenttime = currenttime
                 }, Cmd.none)

calculatecurrent p1 p2 p3 =
    p1 * p2 * p3


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
    if model.currenttime > maxx then
        Sub.none
    else
        Time.every  (Time.second * 2) Propagate

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
      [ h4 [] [text (model.label ++ ":")]
      , div []
          [ input [ type_ "range"
                  , onInput Slide
                  , value <| toString <| model.value
                  , A.max <| toString <| model.max
                  , A.min <| toString <| model.min
                  , step model.step ] []
          , span []
              [input [type_ "number"
                     , onchange TextIn
                     , value <| toString <| model.value
                     , A.max <| toString <| model.max
                     , A.min <| toString <| model.min
                     , step model.step ] []
              ] 
          ]
      ]

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
