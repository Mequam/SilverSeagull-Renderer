module Sphear (someFunc) where

import Codec.Picture (Pixel)
import MathVec

--this represents textural defaults that the surface of our object can use when reflecting light
--NOTE: a is inteanded to be a pixel type and will primarily be used as such in our later functions
data LightInfo a = Light a Int

--this is the data that we store our objects to render as behind the scenes
--light geo contains light data, and then a geomotry object
--NOTE: shapeType is inteanded to be a shape defined by our shape class
data CollisionObject lightType shapeType = LightGeo (LightInfo lightType) shapeType

class (Shape shapeType) where
 --gets the position of the shape
 pos :: (Num numType,Floating numType) => shapeType -> MathVec numType
 --takes a point along the border of the object and returns the normal at that point
 normal :: (Num a,Floating a) => shapeType -> MathVec a -> MathVec a
 --gets the parametric time, if any, that the given direction vector collides with the border
 --the first vector in OffsetChords is the originating position of the second vector 
 colTime :: (Num numType, Fractional numType) => shapeType -> numType -> numType


--gets the reflected vector given a normal and incoming vector, 
--arguments are as follows: normal,incoming vector, outgoing vector
--NOTE: incoming here is centered on the collision point with the normal and going twoards it
reflection :: (Num numType,Floating numType) => MathVec numType -> MathVec numType -> MathVec numType
reflection normal incoming = incoming-(((normal `scaled` (normal`dot`incoming)) `scaled` 2))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
