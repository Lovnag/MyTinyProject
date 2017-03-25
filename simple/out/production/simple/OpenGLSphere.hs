import Graphics.GLU
import Graphics.Rendering.OpenGL
import Data.IORef
import Data.Time.Clock.POSIX

main = do
  getArgsAndInitialize
  createWindow "Sphere(or just some random figure) Floating"
  time <- getTime
  state <- newIORef (0.01, 0.01, 0.01, 0.02 :: GLdouble, 0.02, 0,02, time)
  displayCallback $= display st
  idleCallback $= Just (idle st)
  mainLoop

display state = do
  (x, y, z, _, _, _, _) <- get state
  clear [ColorBuffer]
  currentColor $= Color4 2 10 2 2
  renderPrimitive Polygon $ do
    vertex $ Vertex3 (x   :: GLdouble) y z
    vertex $ Vertex3 (0.1 + x   :: GLdouble) (0.1 + y) (0.1 + z)
    vertex $ Vertex3 (0.05 + x   :: GLdouble) (0.1 + y) (0.05 + z)
    vertex $ Vertex3 (0.1 + x   :: GLdouble) (0.05 + y) (0.05 + z)
    vertex $ Vertex3 (0.05 + x   :: GLdouble) (0.05 + y) (0.1 + z)
    vertex $ Vertex3 (0.1 + x   :: GLdouble) (0.1 + y) (0.05 + z)
  flush

idle state = do
  (x, y, z, speedX, speedY, speedZ, prevTime) <- get state
  time <- getTime
  let newX = x + speedX*(time-prevTime)
      newY = y + speedY*(time-prevTime)
      newZ = z + speedZ*(time-prevTime)
  state $=! (newX, newY, newZ, speedX, speedY, speedZ, time)
  postRedisplay Nothing


getTime :: IO GLdouble
getTime = do
  now <- getPOSIXTime
  return $ fromRational $ toRational now

