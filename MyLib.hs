module MyLib (someFunc) where

import Codec.Picture (Pixel)

--this represents textural defaults that the surface of our object can use when reflecting light
--NOTE: a is inteanded to be a pixel type and will primarily be used as such in our later functions
data LightInfo a = Light a Int

--this is the data that we store our objects to render as behind the scenes
--light geo contains light data, and then a geomotry object
--NOTE: shapeType is inteanded to be a shape defined by our shape class
data CollisionObject lightType shapeType = LightGeo (LightInfo lightType) shapeType

class (Shape shapeType numType) where
 pos :: (Num numType,Fractional numType) => shapeType -> [numType]
 --takes a point along the border of the object and returns the normal at that point
 normal :: [numType] -> [numType]
someFunc :: IO ()
someFunc = putStrLn "someFunc"
