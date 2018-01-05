module PhysicalModels exposing (..)

-- Simple model of the form xdot = a*x
-- and the analytical solution.

import Solvers exposing (euler)

simple p t x =
    p * x


-- A simple integrating process. From Smith and Corripio \S 4.4.1\
-- A first order pump
pump kp taup m t0 f0 =
    (kp*m - f0)/taup

-- A tank with liquid inside. Constant density cancels out.
tank area fi fo t0 h0 =
    (fi - fo)/area

mypump = pump 0.2 0.1

mytank myfi t0 h0 =
    tank 20.0 myfi 12 t0 h0

