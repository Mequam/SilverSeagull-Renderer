{-# LANGUAGE FunctionalDependencies #-} --this is so we can tell haskell that the numType is determined by what the shapeType contains

module Main where

--not really sure what all of these are used for
--that is why this is the hello world for our image processing adventures
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import System.Environment (getArgs)

import qualified Codec.Picture.Types as M

import GeoShape.Shape 
import GeoShape.PreMade.Sphear
import MathVec.MathVec

data Texture = ReflectionColor Float PixelRGB8 | Color PixelRGB8 | Nada deriving (Show,Eq)
data RenderObj shapeObj = ShapeTexture shapeObj Texture deriving (Show,Eq)

textColor :: Texture -> PixelRGB8
textColor (ReflectionColor _ c) = c
textColor (Color c) = c --the given color
textColor _ = PixelRGB8 0 0 0 --black

textReflectionFact :: Texture -> Float
textReflectionFact (ReflectionColor rf _) = rf
textReflectionFact _ = 0

rendText :: RenderObj a -> Texture
rendText (ShapeTexture _ t) = t

scaleColor :: Float -> PixelRGB8 -> PixelRGB8
scaleColor f (PixelRGB8 r g b) = PixelRGB8 (scaleword f r) (scaleword f g) (scaleword f b) where scaleword x y = round ((fromIntegral y)*x)
addColor :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
addColor (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = PixelRGB8 (addWordsCeil r1 r2) (addWordsCeil g1 g2) (addWordsCeil b1 b2)

addWordsCeil :: (Integral a) => a -> a -> a
addWordsCeil x y
 | intTotal > 255 = 255
 | otherwise = x+y
 where
  intTotal = (fromIntegral x) + (fromIntegral y)

main :: IO ()
main = do  
 let shapes = [(ShapeTexture (RadiusPos 5 (Chords [(0),(0),25])) (Color (PixelRGB8 50 50 50))),(ShapeTexture (RadiusPos 20 (Chords [(15),0,50])) (ReflectionColor 0.5 (PixelRGB8 0 0 255))),(ShapeTexture (RadiusPos 20 (Chords [(-15),0,50])) (ReflectionColor 0.1 (PixelRGB8 10 10 10)))] 
 
 --debug code
 putStrLn (show (addColor (PixelRGB8 255 255 255) (absorbColor (PixelRGB8 0 0 255) (absorbColor (PixelRGB8 255 0 0) (PixelRGB8 255 255 255))))) 
 putStrLn "[*] beginning render..." 
 --					generateImae creates a "specific image" class, the ImageRGB8 is a constructor for the generic image data type
 --					that "tags" the image so other parts of the libary know what they are looking at
 --					this creates a DynamicImage that can be used elsewhere with many different functions regaurldess of the underlying type
 savePngImage "./test.png" (ImageRGB8 (generateImage (generator shapes (400) (-500) (-500)) 1000 1000))

--gets the shape that we collide with and the time of that collision
colTimeShapes :: (Shape st a,Eq st) => [RenderObj st] -> OffsetMathVec a -> Maybe (a,RenderObj st)
colTimeShapes ((ShapeTexture fs ftext):shapeArr) ray
 | time == Nothing = nextItr --every collision to the right and ours missed
 | nextItr == Nothing = Just (fromJust time,ShapeTexture fs ftext) --our collision is the only collision so far 
 | (fromJust time) < (fst (fromJust nextItr)) = Just (fromJust time,ShapeTexture fs ftext) --found two collisions, return the smaller 
 | otherwise = nextItr 
 where
  time = (colTime fs ray)
  nextItr = colTimeShapes shapeArr ray
colTimeShapes [ShapeTexture fs ftext] ray --exit condition
 | time == Nothing = Nothing
 | otherwise = Just (fromJust time,ShapeTexture fs ftext)
 where
  time = (colTime fs ray)
colTimeShapes [] _ = Nothing --just in case



--takes a base color representing the surface of an object
--and an input color representing input light
--it then returns the incoming light reflected from the surface
absorbColor :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
absorbColor (PixelRGB8 r g b) (PixelRGB8 rl gl bl) = (PixelRGB8 (multColorChord r rl) (multColorChord g gl) (multColorChord b bl)) where multColorChord a b = round ((fromIntegral a)*(fromIntegral b)/255)

--we need a function that merges colors
--this function recursivly bounces a ray of light around the scene and returns the color from the light
bounceRay :: (Shape st a,Num a,Eq st) => Integer -> Integer -> [RenderObj st] -> OffsetMathVec a -> Maybe (a,RenderObj st) -> PixelRGB8
bounceRay maxCount count shapes ray (Just (time,ShapeTexture s t))
 | count >= maxCount || nextCol == Nothing = (absorbColor (textColor t) ambientColor)
 | otherwise = addColor (absorbColor (textColor t) ambientColor) (scaleColor (textReflectionFact t) (bounceRay maxCount (count+1) shapes reflectedRay nextCol))
 where
  ambientColor = (PixelRGB8 255 100 100)
  lightColor = (PixelRGB8 255 255 255)
  colPoint = ray `fromOffsetScaled` time --get the point of the collision by scaling the ray to the time that we hit
  reflectedDir = reflection (normal s colPoint) (offsetDir ray) 
  reflectedRay = (reflectedDir,colPoint+(signum reflectedDir) `scaled` 0.1)
  nextCol = colTimeShapes shapes reflectedRay
bounceRay _ _ _ _ Nothing = (PixelRGB8 255 0 0) --TODO: this is where we need to check for collision with the light sources

--this function is used in a higher order function to determin the color
--of the pixel chord
generator :: (Shape st a,Num a,Eq st) => [RenderObj st] -> a -> Int -> Int -> Int -> Int-> PixelRGB8
generator renderObjects camaraDist xoff yoff x y
 | col == Nothing = PixelRGB8 0 0 0 --we hit nothing, darkness consumes *^*
 | otherwise = bounceRay 100 0 renderObjects ray col
 where
  ray = (signum (Chords [fromIntegral (x+xoff),fromIntegral (y+yoff),camaraDist,0]),(Chords [0,0,0,0]))
  col = colTimeShapes renderObjects ray
