module Controllers exposing (..)

-- PID
-- A controller with the ability to apply
-- P, I, and/or D components to control action
-- uses the following equation:
-- m(t) = bias + kp*e + ki*integral(e) + kd*derivative(e)

type alias PIDBasic =
    { kp : Float
    , ki : Float
    , kd : Float
    , errorintegral : Float
    , lastE : Float
    , setPoint : Float
    , output : Float
    }

update dt controller feedback =
    let
        controller = c
        error = c.setPoint - feedback
        delta_error = error - c.lastE
        pterm = c.kp*error
        iterm = c.iterm + error * dt
        dterm = c.kd*delta_error/dt
        output = pterm + iterm*c.ki + dterm
    in
        {controller |
             errorintegral = iterm ,
             lastE = error ,
             output = output
        }

             
