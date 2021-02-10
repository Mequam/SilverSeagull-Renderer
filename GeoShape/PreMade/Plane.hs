{-#Language FlexibleInstances#-}
{-#Language MultiParamTypeClasses#-}
{-#Language FlexibleContexts#-}

module GeoShape.PreMade.Plane (Plane (..)) where

import GeoShape.Shape
import MathVec.MathVec

--represents an infinit plane
data Plane numType = InfinitPlane Int numType deriving (Show,Eq)

sumArr :: (Num a) => [a] -> a
sumArr arr = foldr1 (+) arr
instance (Ord numType,Num numType, Floating numType) => Shape (Plane numType) numType where 
 pos (InfinitPlane chord dist) = (unitVec chord) `scaled` dist 
 colTimeLocal (InfinitPlane v _) (dir,pos)
  | t > 0 = Just t
  | otherwise = Nothing
  where
   t = ((-(chordVec v pos))/(chordVec v dir)) --solve for when our given chordinent is zero for the local chords
 normalLocal (InfinitPlane v _) dir
  | chordx (uv*dir) > 0 = uv
  | otherwise = -uv
  where
   uv = (unitVec v)
