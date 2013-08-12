import System.RaspberryPi.GPIO
import Control.Concurrent

main = withGPIO $ do
    setPinFunction Pin11 Output
    forever $ do
        threadDelay 1000000
        writePin Pin11 True
        threadDelay 1000000
        writePin Pin11 False