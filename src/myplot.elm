module MyPlot exposing (..)

{- Module that customizes my elm-plot graphic.  Note that the current
customization does not render well in Internet Explorer. Some issue
with shiftedLabel or shiftedLabel2.
-}

import Svg.Attributes as SA
import Html.Attributes as A
import Plot as P
import Parameters exposing (..)

myseries toDataPoints =
    { axis = P.normalAxis
    , interpolation = P.None
    , toDataPoints = toDataPoints
    }

smallcircle =
    P.dot (P.viewCircle 3.0 "#1d6632")

shiftedLabel : Float -> P.LabelCustomizations
shiftedLabel position =
    { position = position
    , view = P.viewLabel [(P.displace 0 -15)
                         , SA.dominantBaseline "hanging"] (toString position)
    }

shiftedLabel2 : Float -> P.LabelCustomizations
shiftedLabel2 position =
    { position = position
    , view = P.viewLabel [SA.dy "-1em"] (toString position)
    }

{- Customize the axis to use the shifted labels
-}        
myaxis : List Float -> P.Axis
myaxis ticksandlabels =
    P.customAxis <| \summary ->
        { position = P.closestToZero
        , axisLine = Just (P.simpleLine summary)
        , ticks = List.map P.simpleTick ticksandlabels
        , labels = List.map shiftedLabel ticksandlabels
        , flipAnchor = False
        }

tickintervalx =
    maxx/8

ticklocationsx =
    List.map
        (\x -> (toFloat x)*tickintervalx)
        (List.range 0 8)
        
xaxis = myaxis ticklocationsx

pltwidth = 520
pltheight = 300
        
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
