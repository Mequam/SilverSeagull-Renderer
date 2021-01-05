module Main where

--not really sure what all of these are used for
--that is why this is the hello world for our image processing adventures
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import qualified Codec.Picture.Types as M
import qualified MyLib (someFunc)

main :: IO ()
main = do 
 putStrLn "Hello, Haskell!"
 MyLib.someFunc
 --					generateImae creates a "specific image" class, the ImageRGB8 is a constructor for the generic image data type
 --					that "tags" the image so other parts of the libary know what they are looking at
 --					this creates a DynamicImage that can be used elsewhere with many different functions regaurldess of the underlying type
 savePngImage "./test.png" (ImageRGB8 (generateImage generator 100 100))

--this function is used in a higher order function to determin the color
--of the pixel chord
generator :: Int -> Int -> PixelRGB8
generator x _ = PixelRGB8 0 0 (50*(fromIntegral x))
