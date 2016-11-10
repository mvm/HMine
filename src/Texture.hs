module Texture where
import Codec.Image.PNG
import Graphics.Rendering.OpenGL
import Foreign.Ptr (Ptr)
import Data.Array.Storable (withStorableArray)
import Data.Word (Word8)

pixelDataFromArray png = withStorableArray (imageData png) $ (\ptr -> 
        return $ PixelData RGBA UnsignedByte ptr)

loadTextureFromFile :: String -> IO (Maybe TextureObject)
loadTextureFromFile file = do
        img <- loadPNGFile file
        case img of
                Left e -> do putStrLn e ; return Nothing
                Right i -> do putStrLn ("Loaded " ++ file) ; createTexture i

createTexture :: PNGImage -> IO (Maybe TextureObject)
createTexture pngImg = let (w,h) = dimensions pngImg in do
        pixelData <- pixelDataFromArray pngImg
        [tex] <- genObjectNames 1
        textureBinding Texture2D $= Just tex
        build2DMipmaps Texture2D RGBA' (fromIntegral w) (fromIntegral h) pixelData
        textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
        textureFunction $= Modulate
        return $ Just tex
