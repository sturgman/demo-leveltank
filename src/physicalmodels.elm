module PhysicalModels exposing (mypump, pump, simple, tank)

import Solvers exposing (euler)



{- Simple model of the form xdot = a*x. -}


simple p t x =
    p * x



{- A simple integrating process. From Smith and Corripio \S 4.4.1\ A
   first order pump with a time constant (not realistic, but serves the
   purpose.
-}


pump kp taup m t0 f0 =
    (kp * m - f0) / taup



{- A tank with liquid inside. Constant density cancels out. -}


tank area fi fo t0 h0 =
    (fi - fo) / area



{- A particular pump with a gain of 0.2 and a time constant of 5 -}


mypump =
    pump 0.2 5
