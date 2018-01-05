module Parameters exposing (..)

timeinterval : Float
timeinterval = 2.0

timestep : Float
timestep = 0.1

dequesize : Int
dequesize = 120

steplonginterval : Int
steplonginterval = (round (timeinterval/timestep))

minx = 0.0
maxx = (toFloat dequesize) * timeinterval

miny = 0.0
maxy = 1

-- Example data
expdata t = 
    e^(-0.05*t)

-- This is all so that I can prepare the analytical solution
-- so that it works with elm-plot
actydata = List.map (toFloat >> expdata) (List.range 0 240)
actdata = List.map2 (\a b -> (a,b)) (List.map toFloat (List.range 0 240)) actydata

