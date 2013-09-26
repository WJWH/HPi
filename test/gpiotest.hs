import System.RaspberryPi.GPIO
import Control.Concurrent
import Control.Monad

--compile with ghc --make gpiotest.hs ~/bcm2835-1.25/src/bcm2835.o to prevent "reference not found" errors

main = withGPIO $ do
    setPinFunction Pin11 Output
    forever $ do
        threadDelay 1000000
        writePin Pin11 True
        threadDelay 1000000
        writePin Pin11 False