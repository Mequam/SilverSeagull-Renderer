module MathVec (MathVec) where

--basic implimentation of vectors for storage
--stores the numbers and the amount of numbers (or dimension)
data MathVec numType = Chords [numType] deriving (Show)

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


chordx (Chords (fx:_)) = fx
chordy (Chords (_:fy:_)) = fy
chordz (Chords (_:_:fz:_)) = fz

dot :: (Num a,Floating a) => MathVec a -> MathVec a -> a
dot x y = (chordx (x*y))

scale :: (Num a,Floating a) => a -> MathVec a -> MathVec a
scale factor (Chords vec) = (Chords [c*factor|c<-vec])
