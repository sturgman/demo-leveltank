module Parameters exposing (..)

timeinterval : Float
timeinterval = 2.0

timestep : Float
timestep = 0.1

dequesize : Int
dequesize = 120

steplonginterval : Int
steplonginterval = 20

minx = 0.0
maxx = (toFloat dequesize) * timeinterval

miny = 0.0
maxy = 1

