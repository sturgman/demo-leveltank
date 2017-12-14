module Solvers exposing (euler)

euler: Float -> (Float -> Float -> Float) -> Float -> Float
euler dt f t0 x0 =
    let
        dx = (f t0 x0)
    in
        x = x0 + dt*dx
