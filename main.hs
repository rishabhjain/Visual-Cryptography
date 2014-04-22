import Codec.Picture
import System.Cmd (system)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import System.Random

toBinaryImage :: Either String DynamicImage -> Image Pixel8
toBinaryImage = pixelMap (\x -> if x > 128 then 255 else 0) . extractPixel . extractImage where
  extractImage x  = let (Right w)   = x in w
  extractPixel x  = let (ImageY8 w) = x in w

convertToGS imageName = system $ "convert " ++ imageName ++ " -resize 200x200 -gravity Center -extent 200x200 -colorspace Gray " ++ "gs_" ++ imageName  -- ImageMagick

main :: IO ()
main = do
  x <- getArgs
  z <- mapM_ convertToGS x >> mapM (readImage . ("gs_" ++)) x
  system "rm gs_*"
  let [ firstImage, secondImage, message ] = map toBinaryImage z
  let (generatedFirstImage, generatedSecondImage) = getBothImages $ generateFinalImages firstImage secondImage message
  writePng "first.png"  generatedFirstImage
  writePng "second.png" generatedSecondImage

data SuperPixel = S {a :: PixelYA8, b :: PixelYA8, c :: PixelYA8, d :: PixelYA8}

data BinaryPixel = N | W | B deriving (Eq, Show)

type Constraint = Int

fillPixels :: ([BinaryPixel],  StdGen) -> Constraint -> Constraint -> ([BinaryPixel], StdGen) -- White Constraint then Black constraint
fillPixels (x, g) 0 0 = (x,g)
fillPixels (x, g) 0 _ = (map (\s -> if s == N then B else s) x, g)
fillPixels (x, g) _ 0 = (map (\s -> if s == N then W else s) x, g) -- True -> Black
fillPixels (x, g) y z = let (p, newg) = random g in if p then fillPixels (change x B, newg) y (z-1) else fillPixels (change x W, newg) (y-1) z

change :: [BinaryPixel] -> BinaryPixel -> [BinaryPixel]
change (N:y) z = z:y
change (x:y) z = x : change y z

makeSuperP :: [(BinaryPixel ,BinaryPixel)] -> SuperPixel
makeSuperP (x:y:z:t:[]) = S (uncurry PixelYA8 $ convert x) (uncurry PixelYA8 $ convert y) (uncurry PixelYA8 $ convert z) (uncurry PixelYA8 $ convert t) where
  convert (B,W) = (0, 255)
  convert (B,B) = (0,0)
  convert (W,W) = (255,255)
  convert (W,B) = (255,0)

black :: StdGen -> ([BinaryPixel], StdGen)
black g = fillPixels (replicate 4 N, g) 1 3

white :: StdGen -> ([BinaryPixel], StdGen)
white g = fillPixels (replicate 4 N, g) 2 2

condition :: BinaryPixel -> BinaryPixel -> BinaryPixel -> BinaryPixel
condition f s x = if x == f then s else N

generateRandomSuperPixel :: StdGen -> Pixel8 -> Pixel8 -> Pixel8 -> SuperPixel
generateRandomSuperPixel g x y z = case (x, y, z) of
  (0  ,0  ,  0) -> let (bPixel, newG) = black g in let (sBPixel, _) = fillPixels (map (condition W B) bPixel,newG) 1 2 in makeSuperP $ zip bPixel sBPixel
  (0  ,0  ,255) -> let (bPixel, _)    = black g in makeSuperP $ zip bPixel bPixel
  (255,255,255) -> let (wPixel, _)    = white g in makeSuperP $ zip wPixel wPixel
  (255,255,0  ) -> let (wPixel, _)    = white g in makeSuperP $ zip wPixel $ map (\v -> if v == B then W else B) wPixel
  (0  ,255,0  ) -> let (bPixel, newG) = black g in let (wPixel, _)  = fillPixels (map (condition W B) bPixel, newG) 2 1 in makeSuperP $ zip bPixel wPixel
  (0  ,255,255) -> let (bPixel, newG) = black g in let (wPixel, _)  = fillPixels (map (condition W W) bPixel, newG) 1 2 in makeSuperP $ zip bPixel wPixel
  (255,0  ,255) -> let (wPixel, newG) = white g in let (bPixel, _)  = fillPixels (map (condition B B) wPixel, newG) 1 1 in makeSuperP $ zip wPixel bPixel
  (255,0  ,0  ) -> let (wPixel, newG) = white g in let (bPixel, _)  = fillPixels (map (condition W B) wPixel, newG) 1 1 in makeSuperP $ zip wPixel bPixel


generateFinalImages :: Image Pixel8 -> Image Pixel8 -> Image Pixel8 -> Image PixelYA8
generateFinalImages x@(Image w h _) y z = generateImage f (2*w) (2*h) where

  superPixelMap :: M.Map (Int, Int) SuperPixel
  superPixelMap = M.fromList [((i, j), generateRandomSuperPixel (mkStdGen $ i+j) (pixelAt x i j) (pixelAt y i j) (pixelAt z i j)) | i <- [0..w-1], j <- [0..h-1]]

  f :: Int -> Int -> PixelYA8
  f m n | even m && even n = a pixels
        | odd  m && even n = b pixels
        | even m && odd  n = c pixels
        | otherwise        = d pixels where pixels = superPixelMap M.! (div m 2 , div n 2)

getBothImages :: Image PixelYA8 -> (Image PixelYA8, Image PixelYA8)
getBothImages x = (firstImage, secondImage) where
  firstImage  = pixelMap (\(PixelYA8 m n) -> PixelYA8 m $ alpha m) x
  secondImage = pixelMap (\(PixelYA8 m n) -> PixelYA8 n $ alpha n) x
  alpha x     = if x == 255 then 0 else 255
