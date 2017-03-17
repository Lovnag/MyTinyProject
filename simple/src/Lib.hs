module Lib
    ( someFunc
    ) where
 import Data.Array.Vector
someFunc :: IO ()
someFunc = putStrLn "someFunc"

theLinearMovement :: Vector3 -> Double -> Vector3 -> Vector3 -> Vector3

theLinearMovement initialPlacement time initialVelocity acceleration = initialPlacement + initialVelocity |* time + acceleration |* (time*time/2)

theRotationalMovement :: Vector3 -> Double -> Vector3 -> Vector3 -> Vector3

theDots :: [[Vector3]]
theDots = [[<1,1,1>,<3,3,3>,<3,1,1>,<1,3,1>,<3,1,1>,<1,3,3>,<3,1,3>,<3,1,1>],
[<6,6,6>,<8,8,8>,<8,6,6>,<6,8,6>,<8,6,6>,<6,8,8>,<8,6,8>,<8,6,6>]]

theMassCenter :: [Vector3]
theMassCenter = [<2,2,2>,<7,7,7>]

theMass :: [Double]
theMass = [20,20]

theStartingVelocity :: [Vector3]
theStartingVelocity = [<1,1,1>,<2,2,2>]

theForceApplied :: [Vector3]
theForceApplied = [<1,1,1>,<2,2,2>]

timePassing time
      | time == 0 = 0
      otherwise movement time

movement time = rolling time theDots theMass theStartingVelocity theForceApplied impedingCollision

rolling time dots mass startingVelocity forceApplied incomingCollision | incomingCollision dots == true  = collide dots mass startingVelocity forceApplied
                                                                       otherwise doTheMovement

movingASingleCube time dots sumOfStartingVelocity sumOfAcceleration | dots != [] = [theLinearMovement head dots sumOfStartingVelocity sumOfAcceleration, movingASingleCube time tail dots sumOfStartingVelocity sumOfAcceleration]
                                                                    otherwise []