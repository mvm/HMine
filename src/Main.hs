module Main where

import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad

main::IO()
main = do
        initialized <- GLFW.initialize
        when (not initialized) $ fail "Could not initialize GLFW"
        opened <- GLFW.openWindow (GL.Size 640 480) [GLFW.DisplayAlphaBits 8] GLFW.Window
        when (not opened) $ fail "Could not open window"
        GLFW.windowTitle $= "Hello World!"
        
        GLFW.windowSizeCallback $= resizeCallback
        
        gameloop
        
        GLFW.closeWindow
        GLFW.terminate

resizeCallback :: Size -> IO ()
resizeCallback size = do
        GL.viewport $= (Position 0 0, size)
        GL.matrixMode $= Projection
        GL.loadIdentity
        GL.frustum (-1) 1 (-1) 1 1 100

gameloop :: IO ()
gameloop = do
        GL.clearColor $= Color4 0 0 0 0
        GL.clear [GL.ColorBuffer]
        GLFW.swapBuffers
        GLFW.sleep 1