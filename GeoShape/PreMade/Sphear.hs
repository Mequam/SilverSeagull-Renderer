{-#Language FlexibleInstances#-}
{-#Language MultiParamTypeClasses#-}
{-#Language FlexibleContexts#-}

module GeoShape.PreMade.Sphear (Shape,reflection) where

import GeoShape.Shape
import MathVec.MathVec

--the Radius Sphear is said to be directly on the origin
data Sphear numType = Radius numType | RadiusPos numType (MathVec numType) deriving (Show,Eq)

sumArr :: (Num a) => [a] -> a
sumArr arr = foldr1 (+) arr
instance (Ord numType,Num numType, Floating numType) => Shape (Sphear numType) numType where 
 pos (Radius _) = (Chords [0])
 pos (RadiusPos _ pos) = pos
 normalLocal (Radius r) incidentPoint = signum incidentPoint
 normalLocal (RadiusPos r pos) ip = normalLocal (Radius r) ip
 colTimeLocal (RadiusPos r pos) offsetVec = colTimeLocal (Radius r) offsetVec
 colTimeLocal (Radius r) ((Chords dir),(Chords pos)) = singleQuad a b c where (a,b,c) = (sumArr [n^2|n<-dir],sumArr (zipArrays ((*).(*2)) dir pos),(sumArr [p^2|p<-pos])-r^2)

--implimentation of the quadratic formula
quad :: (Ord numType,Num numType,Floating numType) => numType -> numType -> numType -> Maybe (numType,numType)
quad a b c
 | innerRoot >= 0 = Just (((sqrt innerRoot)-b)/bot,((-b)-(sqrt innerRoot))/bot)
 | otherwise = Nothing
 where
  innerRoot = b^2-(4*a*c)
  bot = 2*a

wrapMaybeTup :: Maybe (a,a) -> Maybe (Either a (a,a))
wrapMaybeTup (Just x) = (Just (Right x))
wrapMaybeTup Nothing = Nothing

--returns only posotive values from a either a tuple of values or a single value
posTup :: (Ord numType,Num numType) => Maybe (Either numType (numType,numType)) -> Maybe (Either numType (numType,numType))
posTup (Just (Left x))
 | x >= 0 = (Just (Left x))
 | otherwise = Nothing
posTup (Just (Right (x,y)))
 | x >= 0 && y >= 0 = (Just (Right (x,y)))
 | y >= 0 = (Just (Left y))
 | x >= 0 = (Just (Left x))
 | otherwise = Nothing
posTup Nothing = Nothing

--gets a single value of the given tupal that is the largest of the two (or x in a tie)
lesserTup :: (Eq numType,Ord numType)=> Maybe (Either numType (numType,numType)) -> Maybe numType
lesserTup (Just (Left x)) = Just x
lesserTup (Just (Right (x,y)))
 | x < y || x==y = Just x
 | y < x = Just y
lesserTup Nothing = Nothing

--takes a quadradic and returns only the (greatest, positive value) or nothing
--singleQuad = (greaterTup.(posTup.(wrapMaybeTup.quad)))
--singleQuad :: (Ord numType,Num numType,Floating numType) => numType -> numType -> numType -> Maybe numType
singleQuad a b c = lesserTup (posTup (wrapMaybeTup (quad a b c)))

--for debuging with ghci
circle = (RadiusPos 3 (Chords [0,0]))
offsetRay = ((Chords [(-1),(-4)]),(Chords [4,10])) 
