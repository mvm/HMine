module FontRenderer where
import Codec.Image.PNG

type Font = PNGImage

loadFont :: String -> IO (Either String Font)
loadFont = loadPNGFile
