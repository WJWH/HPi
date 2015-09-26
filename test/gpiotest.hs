import System.RaspberryPi.GPIO
import Control.Concurrent
import Control.Monad

-- Due to the library accessing /dev/mem, the compiled program should be run with sudo.

-- This program switches pin 11 on and off, with roughly a second between state transitions. Attach a LED in series
-- with a resistor between pin 11 and GND to see the LED blinking.

main = withGPIO $ do
    setPinFunction Pin11 Output
    forever $ do
        threadDelay 1000000
        writePin Pin11 True
        threadDelay 1000000
        writePin Pin11 False