module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Control.Monad

type Time = Double
data Universe = NewUniverse {
        isRunning :: Bool -- datos, datos, datos
        }

bigBang :: Universe
bigBang = NewUniverse {isRunning = True} -- initial state of the game

update :: Universe -> Universe
update univ = NewUniverse {isRunning = not $ isRunning univ}

render :: Universe -> Time -> IO ()
render _ _ = do
        clearColor $= Color4 0 0 0 0
        clear [ColorBuffer]
        swapBuffers
        sleep 1

maxSkipTicks :: Int
maxSkipTicks = 5

updatesPerSecond :: Double
updatesPerSecond = 25

skipTicks :: Double
skipTicks = 1000/updatesPerSecond

updateLoop :: Universe -> Time -> Int -> IO (Universe,Time)
updateLoop state nextTick loops = get time >>= (\now -> if
        now > nextTick && loops < maxSkipTicks then do
                sleep 0.001
                updateLoop (update state) (nextTick + skipTicks) (loops + 1)
              else
                return (state, nextTick)
                )

gameLoop :: Universe -> Time -> IO Universe
gameLoop universe nextTick = if isRunning universe then do
        (newUniverse,timeElapsed) <- updateLoop universe nextTick 0
        now <- get time
        let dt = (now + timeElapsed - skipTicks)/timeElapsed in render newUniverse dt
        gameLoop newUniverse timeElapsed
       else
        return universe

main::IO()
main = do
        initialized <- initialize
        when (not initialized) $ fail "Could not initialize GLFW"
        opened <- openWindow (Size 845 480) [DisplayAlphaBits 24] Window
        when (not opened) $ fail "Could not open window"
        windowTitle $= "Hello World!"
        
        windowSizeCallback $= resizeCallback

        shadeModel $= Smooth
        depthFunc $= Just Lequal
        lighting $= Enabled
        
        beginningOfTime <- get time
        _ <- gameLoop bigBang beginningOfTime
        
        closeWindow
        terminate

resizeCallback :: Size -> IO ()
resizeCallback size = do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        frustum (-1) 1 (-1) 1 1 100