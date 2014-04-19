import Codec.Picture
import System.Cmd (system)
import System.Environment (getArgs)

toBinaryImage :: Either String DynamicImage -> Image PixelYA8
toBinaryImage = pixelMap (\x -> if x > 128 then PixelYA8 255 100 else PixelYA8 0 100) . extractPixel . extractImage where
  extractImage x  = let (Right w)   = x in w
  extractPixel x  = let (ImageY8 w) = x in w

main = do
  imageName <- getArgs
  system $ "convert " ++ head imageName ++ " -resize 600x600 -gravity Center -extent 600x600 -colorspace Gray " ++ "gs_" ++ head imageName  -- ImageMagick
  gsImage <- readImage $ "gs_" ++ head imageName
  system $ "rm " ++ "gs_" ++ head imageName
  writePng ("binary" ++ takeWhile (/= '.') (head imageName) ++ ".png") $ toBinaryImage gsImage
