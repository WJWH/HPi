import System.RaspberryPi.GPIO
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word

--When compiling this, make sure to include the path to the bcm2835 library to the compilerto prevent
--"reference not found" errors, ie:
--ghc --make i2ctest.hs ~/bcm2835-1.25/src/bcm2835.o
--Obviouly, this assumes you installed the bcm2835 library in "~/bcm2835-1.25". Due to the library accessing
-- /dev/mem, the compiled program should be run with sudo.

--This program is designed to work with an 8*8 bicolor led matrix from Adafruit (http://www.adafruit.com/products/902)
--It makes some fancy patterns and demonstrates some more advanced Haskell concepts (mostly the StateT monad transformer)

adress :: Address
adress = 0x70 --change this of your I2C device has a different adress

--some useful data types
data LedState = Off | Green | Yellow | Red deriving (Show,Eq,Enum)
type MatrixState = Array (Int,Int) LedState
type Matrix = StateT MatrixState IO

main = withGPIO . withI2C . (flip evalStateT (entireMatrix Off)) $ do --function composition is pretty awesome.
    writeSingleByte 0x21 -- switches on the internal oscillator of the LED backpack
    writeSingleByte 0x81 -- turns on display, sets blink rate to "not blinking"
    writeSingleByte 0xEF -- sets brightness to maximum
    forever $ do
        clearMatrix --switches all pixels to Off
        --fill up the entire array pixel by pixel, waiting a bit after each one
        forM_ [(x,y)| x <- [0..7], y <- [0..7]] (\xy -> writePixel Green xy >> shortDelay)
        --some wild multicolor stuff
        replicateM 5 $ forM_ [Green, Yellow, Red, Yellow] (\ls -> put (entireMatrix ls) >> writeMatrix >> shortDelay)
        -- a bit of dimming
        replicateM 5 $ forM_ ([15,14..0]++[1..14]) (\b -> setBrightness b >> shortDelay)

--a matrix where all LEDs have the same LedState
entireMatrix :: LedState -> MatrixState
entireMatrix c = array ((0,0),(7,7)) [((x,y), c)| x <- [0..7], y <- [0..7]]

--clears the matrix, disables all LEDs
clearMatrix :: Matrix ()
clearMatrix = do
    put $ entireMatrix Off
    writeMatrix

--sets a pixel to a certain color
writePixel :: LedState -> (Int, Int) -> Matrix ()
writePixel c (x, y) = do
    modify (\a -> a//[((x, y), c)])
    writeMatrix

--sets display brightness, this is done by using the magic word 0xE0 bitwise or-ed with the brightness you want
setBrightness :: Word8 -> Matrix ()
setBrightness b
    | (b > 15)  = go 15
    | otherwise = go b
    where go x = writeSingleByte (0xE0 .|. x) --0xEX is the magic word for brightness

--write a single byte, useful for setting brightness, turning on oscillator, etc
writeSingleByte :: Word8 -> Matrix ()
writeSingleByte w = liftIO $ writeI2C adress (BS.singleton w)

--Compared with the adafruit version, the writeMatrix function seems rather complicated. This is because the adafruit keeps a much
--more lowlevel version of the state in memory.
writeMatrix :: Matrix ()
writeMatrix = do
    matrixState <- get
    liftIO $ writeI2C adress (toI2cWords matrixState)

--Pure function to translate from the state matrix to a string of Word8's that can be sent over I2C.
--First comes 0x00, a magic value to tell the LED backpack we're writing to memory, then comes the data.
--For every column there are two Word8's, one for the green LEDs and one for the red LEDs. To get a yellow
--pixel turn on both green and red. The individual bits in the Word8 determine if the LED at the row
--corresponding to the bit location is turned on or not.
toI2cWords :: MatrixState -> BS.ByteString
toI2cWords s = BS.pack $ 0x00 : (merge greenbytes redbytes)
    where   greenbytes  = map greenbits [0..7]
            redbytes    = map redbits   [0..7]
            greenbits y = foldr (.|.) 0 [ shiftL 1 x | x <- [0..7], (s!(x,y) == Green) || (s!(x,y) == Yellow)]
            redbits   y = foldr (.|.) 0 [ shiftL 1 x | x <- [0..7], (s!(x,y) == Red)   || (s!(x,y) == Yellow)]

--a little helper function to merge two lists
merge [] ys = ys
merge (x:xs) ys = x : merge ys xs

--waits for 0.2 seconds, in the Matrix monad
shortDelay :: Matrix ()
shortDelay = liftIO $ threadDelay 200000