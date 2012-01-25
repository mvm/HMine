module FontRenderer where
import Codec.Image.PNG
import Graphics.Rendering.OpenGL
import Foreign.Ptr
import Data.Word

type Font = PNGImage

createTextureFromPtr :: Size -> PixelData Word8 -> IO (Maybe TextureObject)
createTextureFromPtr (Size x y) pxd = do
        [tex] <- genObjectNames 1
        textureBinding Texture2D $= Just tex
        build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pxd
        textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
        textureFunction $= Modulate
        return $ Just tex