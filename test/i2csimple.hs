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
--ghc --make i2csimple.hs ~/bcm2835-1.25/src/bcm2835.o
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

main = withGPIO . withI2C $ do --function composition is pretty awesome.
    print "Starting"
    writeI2C adress $ BS.singleton (0x81)--display on
    writeI2C adress $ BS.singleton (0x21)--start oscillator
    writeI2C adress $ BS.singleton (0xEF)--start oscillator
    writeI2C adress $ BS.pack (0x00 : repeat 16 0xFF) --write an all yellow display
    liftIO $ print "Exiting"
