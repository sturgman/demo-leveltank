module Solvers exposing (euler)

{- A module that contains solvers. Currently only a simple Euler
   integrator is implemented. Shortest module ever?
-}


euler : Float -> (Float -> Float -> Float) -> Float -> Float -> Float
euler dt f t0 x0 =
    let
        dx =
            f t0 x0
    in
    x0 + dt * dx
