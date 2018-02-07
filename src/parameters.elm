module Parameters exposing (..)

{- Defines and calculates a bunch of parameters used in the
simulation.
-}

import List

timeinterval : Float
timeinterval = 2.0

setPoint = 2.5

timestep : Float
timestep = 0.1

dequesize : Int
dequesize = 60

steplonginterval : Int
steplonginterval = (round (timeinterval/timestep))

minx = 0.0
maxx = 120.0 -- (toFloat dequesize) * timeinterval

miny = 0.0
maxy = 5.0

-- Set point
expdata t = setPoint

-- Prepare the data to show the set point in the plot.
actydata = List.map (toFloat >> expdata) (List.range 0 (round maxx))
actdata = List.map2 (\a b -> (a,b)) (List.map toFloat (List.range 0 (round maxx))) actydata


-- Output flow list
outflowpattern = (List.repeat 50 10.0) ++
                 (List.repeat 300 14.0) ++
                 (List.repeat 300 6.0) ++
                 (List.repeat 50 5.0) ++
                 (List.repeat 300 12.0) ++
                 (List.repeat 200 10.0)

transgain = 20.0

initiae = 300.0
        
