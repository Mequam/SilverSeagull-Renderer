{-# LANGUAGE FlexibleInstances #-} --requred to use our class
{-# LANGUAGE MultiParamTypeClasses #-} --this is so that we can use
{-# LANGUAGE FunctionalDependencies #-} --this is so we can tell haskell that the numType is determined by what the shapeType contains
module GeoShape.Shape (Shape (..),reflection) where


import MathVec.MathVec
class (Ord numType,Num numType,Floating numType) => Shape shapeType numType | shapeType -> numType where
 --gets the position of the shape
 pos :: shapeType -> MathVec numType
 --takes a point along the border of the object and returns the normal at that point
 normal :: shapeType -> MathVec numType -> MathVec numType
 --gets the parametric time, if any, that the given direction vector collides with the border
 --the first vector in OffsetChords is the originating position of the second vector 
 colTime :: shapeType -> OffsetMathVec numType -> Maybe numType


--gets the reflected vector given a normal and incoming vector, 
--arguments are as follows: normal,incoming vector, outgoing vector
--NOTE: incoming here is centered on the collision point with the normal and going twoards it
reflection :: (Num numType,Floating numType) => MathVec numType -> MathVec numType -> MathVec numType
reflection normal incoming = incoming-(((normal `scaled` (normal`dot`incoming)) `scaled` 2))
