import System.RaspberryPi.GPIO
import Control.Concurrent
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word

--When compiling this, make sure to include the path to the bcm2835 library to the compilerto prevent
--"reference not found" errors, ie:
--ghc --make spitest.hs ~/bcm2835-1.25/src/bcm2835.o
--Obviouly, this assumes you installed the bcm2835 library in "~/bcm2835-1.25". Due to the library accessing
-- /dev/mem, the compiled program should be run with sudo.

--This program is designed to work with an FRAM memory circuit from Adafruit (http://www.adafruit.com/product/1897)
--The chip select pin of the FRAM should be on CS1

main = withGPIO . withSPI $ do
    chipSelectSPI CS1   --set the Chip select pin to the CS1 pin
    setChipSelectPolaritySPI CS1 False --The FRAM chip needs CS to be puled low for transactions
    setDataModeSPI (False,False) --The FRAM chip can handle either datamode 0,0 or 1,1
    --for a deeper explanation of the values used below, see the datasheet of the FRAM chip
    --the "2" means write, the two zeroes mean memory adress zero and the 12 is the value actually written
    transferManySPI [2,0,0,12] 
    --the "3" means read, the next two zeroes mean memory adress zero and the last one is ignored by the chip but is necessary 
    --to keep the transaction open for the read
    results <- transferManySPI [3,0,0,0]
    print . last $ results -- should print "12"
