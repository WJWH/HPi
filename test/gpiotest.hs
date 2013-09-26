import System.RaspberryPi.GPIO
import Control.Concurrent
import Control.Monad

--compile with ghc --make gpiotest.hs ~/bcm2835-1.25/src/bcm2835.o to prevent "reference not found" errors

--This program switches pin 11 on and off, with roughly a second between state transitions. Attach a LED in series
--with a resistor between pin 11 and GND to see the LED blinking.

main = withGPIO $ do
    setPinFunction Pin11 Output
    forever $ do
        threadDelay 1000000
        writePin Pin11 True
        threadDelay 1000000
        writePin Pin11 False