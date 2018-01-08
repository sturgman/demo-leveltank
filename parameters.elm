module Parameters exposing (..)

timeinterval : Float
timeinterval = 2.0

timestep : Float
timestep = 0.1

dequesize : Int
dequesize = 60

steplonginterval : Int
steplonginterval = (round (timeinterval/timestep))

minx = 0.0
maxx = (toFloat dequesize) * timeinterval

miny = 0.0
maxy = 1

-- Example data
expdata t = 2.5

-- This is all so that I can prepare the analytical solution
-- so that it works with elm-plot
actydata = List.map (toFloat >> expdata) (List.range 0 (round maxx))
actdata = List.map2 (\a b -> (a,b)) (List.map toFloat (List.range 0 (round maxx))) actydata

