module MathVec.MathVec (MathVec (Chords),dot,OffsetMathVec,scaled,chords,chordx,chordy,chordz,zipArrays) where

--this vector class impliments math like number vectors in haskell

--basic implimentation of vectors for storage
--stores the numbers and the amount of numbers (or dimension)
--chords is a basic unit vector centered at the origin
--OffsetChords is a vector centered at the given offset
data MathVec numType = Chords [numType] deriving (Show,Eq)

--A simple type for a vector which has a positional component stored in the second vector of the tuple
type OffsetMathVec numType = (MathVec numType,MathVec numType)

--ths is a function that performs operations on arrays in parallel
--think about it as lining up the arrays and then performing
--the given function on each tuple of the array
zipArrays :: (a->a->a) -> [a] -> [a] -> [a]
zipArrays f (f1:[]) (f2:[]) = [f1`f`f2]
zipArrays f (f1:arr1) (f2:[]) = (f1`f`f2):arr1
zipArrays f (f1:[]) (f2:arr2) = (f1`f`f2):arr2
zipArrays f (f1:arr1) (f2:arr2) = (f1`f`f2) : (zipArrays f arr1 arr2)

--adds two arrays together
sumParallel :: (Num a) => [a] -> [a] -> [a]
sumParallel = zipArrays (+)

--multiply arrays in parallel
multParallel :: (Num a) => [a] -> [a] -> [a]
multParallel = zipArrays (*)

--negates an array
negateArr :: (Num a) => [a] -> [a]
negateArr (fv:[]) = [negate fv]
negateArr (fv:arrv) = (negate fv):(negateArr arrv)

--sums an array
sumArr :: (Num a) => [a] -> a
sumArr = foldr1 (+)

--gets the distance of an array
distArr :: (Num a,Floating a) => [a] -> a
distArr arr = sqrt (sumArr [c^2 | c<-arr])

instance (Num numType,Floating numType) => Num (MathVec numType) where
 (+) (Chords (arrV1)) (Chords (arrV2)) = Chords (sumParallel arrV1 arrV2)
 negate (Chords arrV1) = Chords (negateArr arrV1)
 --speed up the absolute value function a bit
 abs (Chords [x]) = Chords [abs x] 
 abs (Chords arr) = (Chords [distArr arr]) 

 signum (Chords arrV1) = Chords [ c/dist |c<-arrV1] where dist = (distArr arrV1)
 fromInteger x = Chords [fromInteger x]
 --this is actually the dot product of the two vectors
 (*) (Chords arr1) (Chords arr2) = Chords [(sumArr (multParallel arr1 arr2))]

--simple syntactic sugar functions for grabbing the arrays
chordx (Chords (fx:_)) = fx
chordy (Chords (_:fy:_)) = fy
chordz (Chords (_:_:fz:_)) = fz
chords (Chords arr) = arr

--syntactic sugar function to get the first chord of the dot product
dot :: (Num numType,Floating numType) => MathVec numType -> MathVec numType -> numType
dot v1 v2 = chordx (v1*v2)
scaled :: (Num a,Floating a) => MathVec a  -> a -> MathVec a
scaled (Chords vec) factor = (Chords [c*factor|c<-vec])
