import Data.Array.Vector
module SphereLib where

type Speed = Vector3
type Acceleration = Vector3
type Force = Vector3
type Impulse = Vector3
type Placement = Vector3
type Time = Double
type Mass = Double

type Radius = Double

theLinearMovement :: Placement -> Time -> Speed -> Acceleration -> Placement

data SphereDynamics = SphDyn {Center :: Placement , Spd :: Speed , Rad :: Radius}

Moving :: SphereDynamics -> Time -> SphereDynamics
Moving sphere 1 = SphDyn {Center sphere + Spd sphere , Spd sphere , Rad sphere}
Moving sphere time = Moving (Moving sphere 1) time-1
Moving sphere 0 = undefined