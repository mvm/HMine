module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Control.Monad

import FontRenderer

type Time = Double
data Universe = NewUniverse {
        isRunning :: Bool -- datos, datos, datos
        }

bigBang :: Universe
bigBang = NewUniverse {isRunning = True} -- initial state of the game

update :: Universe -> IO Universe
update univ = do
        x <- shouldClose univ
        return $ NewUniverse {isRunning = x}

shouldClose :: Universe -> IO Bool
shouldClose _ = do
        pressed <- getKey ESC
        windowOpen <- getParam Opened
        return $ not (pressed == Press) && windowOpen

render :: Universe -> Time -> IO ()
render _ _ = do
        clearColor $= Color4 0 0 0 0
        clear [ColorBuffer]
        swapBuffers

maxSkipTicks :: Int
maxSkipTicks = 5

updatesPerSecond :: Double
updatesPerSecond = 25

skipTicks :: Double
skipTicks = 1/updatesPerSecond

updateLoop :: Universe -> Time -> Int -> IO (Universe,Time)
updateLoop state nextTick loops = get time >>= (\now -> if
        now > nextTick && loops < maxSkipTicks then do
                newUniverse <- update state
                updateLoop newUniverse (nextTick + skipTicks) (loops + 1)
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
        windowCloseCallback $= return True

        shadeModel $= Smooth
        depthFunc $= Just Lequal
        lighting $= Enabled
        texture Texture2D $= Enabled
        
        beginningOfTime <- get time
        _ <- gameLoop bigBang beginningOfTime
        
        closeWindow
        terminate

resizeCallback :: Size -> IO ()
resizeCallback size@(Size w h) = let [w',h'] = map fromIntegral [w,h]; ratio = w'/h'  in do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        frustum (-1) 1 (negate ratio) (ratio) 1 100
        matrixMode $= Modelview 0
