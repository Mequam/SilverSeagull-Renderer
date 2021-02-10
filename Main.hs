{-# LANGUAGE FlexibleInstances #-} --requred to use our class
{-# LANGUAGE MultiParamTypeClasses #-} --this is so that we can use
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
import GeoShape.PreMade.Plane
import MathVec.MathVec

data Texture = ColorArr [PixelRGB8] | ReflectionColor Float PixelRGB8 | Color PixelRGB8 | Nada deriving (Show,Eq)
--container for shapes that we support
data SupportedShape a = Circle (Sphear a) | Surface (Plane a) deriving (Show,Eq)

data RenderObj a = ShapeTextureShade (SupportedShape a) Texture (Shader)

--theres got to be a better way to do this
instance (Ord a,Num a,Floating a) => Shape (SupportedShape a) a where
 pos (Circle s) = pos s
 pos (Surface s) = pos s
 normalLocal (Surface s) = normalLocal s
 normalLocal (Circle s) = normalLocal s
 colTimeLocal (Surface s) = colTimeLocal s
 colTimeLocal (Circle s) = colTimeLocal s

instance (Eq a) => Eq (RenderObj a) where
 (==) (ShapeTextureShade s1 t1 _) (ShapeTextureShade s2 t2 _) = s1==s2 && t1==t2
instance (Show a) => Show (RenderObj a) where
 show (ShapeTextureShade s1 t1 _) = "ShapeTextureShader ("++(show s1) ++ ") (" ++ (show t1) ++ ")"
textColor :: Texture -> PixelRGB8
textColor (ReflectionColor _ c) = c
textColor (Color c) = c --the given color
textColor _ = PixelRGB8 0 0 0 --black

textReflectionFact :: Texture -> Float
textReflectionFact (ReflectionColor rf _) = rf
textReflectionFact _ = 0

rendText :: RenderObj a -> Texture
rendText (ShapeTextureShade _ t _) = t

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



--(absorbColor (textColor t) ambientColor) 
--

--custom Shaders
mainShader :: Texture -> PixelRGB8 -> PixelRGB8
mainShader t lc = addColor (absorbColor (textColor t) ambientColor) (scaleColor (textReflectionFact t) lc) where ambientColor = PixelRGB8 150 150 150 
 
main :: IO ()
main = do  
 --let shapes = [(ShapeTextureShade (Surface (InfinitPlane 1 (-5))) (ReflectionColor 0.25 (PixelRGB8 0 0 200)) (mainShader))] 
 let shapes = [(ShapeTextureShade (Surface (InfinitPlane 1 (2))) (ReflectionColor 0.8 (PixelRGB8 10 10 10)) (mainShader)),(ShapeTextureShade (Circle (RadiusPos 1 (Chords [0,(-1),(5)]))) (ReflectionColor 0.25 (PixelRGB8 0 0 200)) (mainShader)),(ShapeTextureShade (Circle (RadiusPos 1 (Chords [0,1,(5)]))) (ReflectionColor 1 (PixelRGB8 150 150 150)) (mainShader)),(ShapeTextureShade (Circle (RadiusPos 1 (Chords [2,(0),(5)]))) (ReflectionColor 0.25 (PixelRGB8 200 0 0)) (mainShader)),(ShapeTextureShade (Circle (RadiusPos 1 (Chords [(-2),(0),(5)]))) (ReflectionColor 0.25 (PixelRGB8 0 200 0)) (mainShader))] 
 
 --debug code 
 putStrLn (show (colTime (InfinitPlane 0 (-2)) ((Chords [1,0]),(Chords [0,0]))))
 putStrLn "[*] beginning render..." 
 --					generateImae creates a "specific image" class, the ImageRGB8 is a constructor for the generic image data type
 --					that "tags" the image so other parts of the libary know what they are looking at
 --					this creates a DynamicImage that can be used elsewhere with many different functions regaurldess of the underlying type
 savePngImage "./test.png" (ImageRGB8 (generateImage (generator shapes (1000) (-1000) (-500)) 2000 2000))

--gets the shape that we collide with and the time of that collision
colTimeShapes :: (Num a,Ord a,Floating a,Eq a) => [RenderObj a] -> OffsetMathVec a -> Maybe (a,RenderObj a)
colTimeShapes ((ShapeTextureShade fs ftext shade):shapeArr) ray
 | time == Nothing = nextItr --every collision to the right and ours missed
 | nextItr == Nothing = Just (fromJust time,ShapeTextureShade fs ftext shade) --our collision is the only collision so far 
 | (fromJust time) < (fst (fromJust nextItr)) = Just (fromJust time,ShapeTextureShade fs ftext shade) --found two collisions, return the smaller 
 | otherwise = nextItr 
 where
  time = (colTime fs ray)
  nextItr = colTimeShapes shapeArr ray
colTimeShapes [ShapeTextureShade fs ftext shade] ray --exit condition
 | time == Nothing = Nothing
 | otherwise = Just (fromJust time,ShapeTextureShade fs ftext shade)
 where
  time = (colTime fs ray)
colTimeShapes [] _ = Nothing --just in case



--takes a base color representing the surface of an object
--and an input color representing input light
--it then returns the incoming light reflected from the surface
absorbColor :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
absorbColor (PixelRGB8 r g b) (PixelRGB8 rl gl bl) = (PixelRGB8 (multColorChord r rl) (multColorChord g gl) (multColorChord b bl)) where multColorChord a b = round ((fromIntegral a)*(fromIntegral b)/255)

type Shader = Texture -> PixelRGB8 -> PixelRGB8 


--we need a function that merges colors
--this function recursivly bounces a ray of light around the scene and returns the color from the light
bounceRay :: (Num a,Eq a,Ord a,Floating a) => Integer -> Integer -> [RenderObj a] -> OffsetMathVec a -> Maybe (a,RenderObj a) -> PixelRGB8
bounceRay maxCount count shapes ray (Just (time,ShapeTextureShade s t shapeShader))
 | count >= maxCount || nextCol == Nothing = shapeShader t lightColor
 | otherwise = shapeShader t (bounceRay maxCount (count+1) shapes reflectedRay nextCol)
 where
  lightColor = (PixelRGB8 0 0 0) 
  colPoint = ray `fromOffsetScaled` time --get the point of the collision by scaling the ray to the time that we hit
  reflectedDir = reflection (normal s colPoint) (offsetDir ray) 
  reflectedRay = (reflectedDir,colPoint+(signum reflectedDir) `scaled` 0.1)
  nextCol = colTimeShapes shapes reflectedRay
bounceRay _ _ _ _ Nothing = (PixelRGB8 255 0 0) --TODO: this is where we need to check for collision with the light sources

--this function is used in a higher order function to determin the color
--of the pixel chord
generator :: (Num a,Floating a,Ord a,Eq a) => [RenderObj a] -> a -> Int -> Int -> Int -> Int-> PixelRGB8
generator renderObjects camaraDist xoff yoff x y
 | col == Nothing = PixelRGB8 0 0 0 --we hit nothing, darkness consumes *^*
 | otherwise = bounceRay 100 0 renderObjects ray col
 where
  ray = (signum (Chords [fromIntegral (x+xoff),fromIntegral (y+yoff),camaraDist,0]),(Chords [0,0,0,0]))
  col = colTimeShapes renderObjects ray
