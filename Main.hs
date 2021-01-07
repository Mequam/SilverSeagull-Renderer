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

data Texture = Color PixelRGB8 | Nada deriving (Show,Eq)
data RenderObj shapeObj = ShapeTexture shapeObj Texture deriving (Show,Eq)

textColor :: Texture -> PixelRGB8
textColor (Color c) = c --the given color
textColor _ = PixelRGB8 0 0 0 --black

rendText :: RenderObj a -> Texture
rendText (ShapeTexture _ t) = t

main :: IO ()
main = do  
 let shapes = [(ShapeTexture (RadiusPos 40 (Chords [0,0,50,10])) (Color (PixelRGB8 0 0 255))),(ShapeTexture (RadiusPos 90 (Chords [30,30,100,(-10)])) (Color (PixelRGB8 0 255 0)))]
 let ray = ((Chords [0,0,1]),(Chords [0,0,0]))
 putStrLn (show (colTimeShapes shapes ray))
 putStrLn "[*] beginning render..." 
 --					generateImae creates a "specific image" class, the ImageRGB8 is a constructor for the generic image data type
 --					that "tags" the image so other parts of the libary know what they are looking at
 --					this creates a DynamicImage that can be used elsewhere with many different functions regaurldess of the underlying type
 savePngImage "./test.png" (ImageRGB8 (generateImage (generator shapes (250) (-500) (-500)) 1000 1000))

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

--this function is used in a higher order function to determin the color
--of the pixel chord
generator :: (Shape st a,Num a,Eq st) => [RenderObj st] -> a -> Int -> Int -> Int -> Int-> PixelRGB8
generator renderObjects camaraDist xoff yoff x y
 | col == Nothing = PixelRGB8 0 0 0 --we hit nothing, darkness consumes *^*
 | otherwise = textColor (rendText (snd (fromJust col)))
 where
  ray = (signum (Chords [fromIntegral (x+xoff),fromIntegral (y+yoff),camaraDist,0]),(Chords [0,0,0,0]))
  col = colTimeShapes renderObjects ray
