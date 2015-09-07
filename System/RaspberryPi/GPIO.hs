{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

-- |Library for controlling the GPIO pins on a Raspberry Pi (or any system using the Broadcom 2835 SOC). It is constructed
-- as a FFI wrapper over the BCM2835 library by Mike McCauley.
module System.RaspberryPi.GPIO (
    -- *Data types
    Pin(..),
    PinMode(..),
    LogicLevel,
    Address,
    -- *General functions
    withGPIO,
    -- *GPIO specific functions
    setPinFunction,
    readPin,
    writePin,
    -- *I2C specific functions
    withI2C,
    setI2cClockDivider,
    setI2cBaudRate,
    writeI2C,
    readI2C,
    writeReadI2C
    ) where

-- FFI wrapper over the I2C portions of the BCM2835 library by Mike McCauley, also some utility functions to
-- make reading and writing simpler

import Control.Applicative ((<$>))
import Control.Exception
import Foreign
import Foreign.C
import Foreign.C.String
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Tuple
import GHC.IO.Exception

------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------- Data types ---------------------------------------------------------------------------

data Pin =  -- |Pins for the P1 connector of the V2 revision of the Raspberry Pi
            Pin03|Pin05|Pin07|Pin08|Pin10|Pin11|Pin12|Pin13|Pin15|Pin16|Pin18|Pin19|Pin21|Pin22|Pin23|Pin24|Pin26|
            -- |Pins for the P5 connector of the V2 revision of the Raspberry Pi
            PinP5_03|PinP5_04|PinP5_05|PinP5_06|
            -- |Pins for the P1 connector of the V1 revision of the Raspberry Pi
            PinV1_03|PinV1_05|PinV1_07|PinV1_08|PinV1_10|PinV1_11|PinV1_12|PinV1_13|PinV1_15|PinV1_16|PinV1_18|PinV1_19|PinV1_21|
            PinV1_22|PinV1_23|PinV1_24|PinV1_26
            deriving (Eq,Show)

-- |A GPIO pin can be either set to input mode, output mode or an alternative mode.
data PinMode = Input | Output | Alt0 | Alt1 | Alt2 | Alt3 | Alt4 | Alt5 deriving (Eq,Show)

instance Enum PinMode where -- bit strange, but just deriving Enum doesn't work because the numbers don't monotonically ascend
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table = [(Input, 0), (Output, 1), (Alt0, 4), (Alt1, 5), (Alt2, 6), (Alt3, 7), (Alt4, 3), (Alt5, 2)]

-- |This describes the address of an I2C slave.
type Address = Word8 --adress of an I2C slave

-- |Either high or low.
type LogicLevel = Bool

------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------ Foreign imports -------------------------------------------------------------------------

----------------------------------------- Library functions ------------------------------------------------------------------------
--initialises /dev/mem and obtaining the proper pointers to device registers. Don't use any other functions if this fails!
foreign import ccall unsafe "bcm2835.h bcm2835_init" initBCM2835 :: IO Int
--deallocates any memory, closes /dev/mem and stops the library in general
foreign import ccall unsafe "bcm2835.h bcm2835_close" stopBCM2835 :: IO Int
--sets debug level
foreign import ccall unsafe "bcm2835.h bcm2835_set_debug" setDebugBCM2835 :: CUChar -> IO ()

---------------------------------------- Basic GPIO functions ----------------------------------------------------------------------
-- setFunction (input/output)
foreign import ccall unsafe "bcm2835.h bcm2835_gpio_fsel" c_setPinFunction    :: CUChar -> CUChar -> IO ()
-- setPin (zet een outputpin hoog/laag)
foreign import ccall unsafe "bcm2835.h bcm2835_gpio_write" c_writePin         :: CUChar -> CUChar -> IO ()
-- readPin (geeft weer of een pin hoog/laag is)
foreign import ccall unsafe "bcm2835.h bcm2835_gpio_lev" c_readPin            :: CUChar -> IO CUChar

------------------------------------------- I2C functions --------------------------------------------------------------------------
--inits the i2c pins
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_begin" initI2C   :: IO ()
--resets the i2c pins
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_end"   stopI2C   :: IO ()

--sets the slave address used
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_setSlaveAddress" c_setSlaveAddressI2C ::   CUChar -> IO ()
--sets the I2C bus clock divider (and thus, the speed)
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_setClockDivider" c_setClockDividerI2C ::   CUShort -> IO ()
--sets the I2C bus baud rate (100000 for the default 100khz)
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_set_baudrate"    c_setBaudRateI2C ::       CUInt -> IO ()

-- writes some bytes to the bus
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_write" c_writeI2C :: CString -> CUInt -> IO CUChar
--read some bytes from the bus
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_read" c_readI2C :: CString -> CUShort -> IO CUChar
--reads a certain register with the repeated start method
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_read_register_rs" c_writeReadI2C :: CString -> CString -> CUShort -> IO CUChar

------------------------------------------- SPI functions --------------------------------------------------------------------------
--inits the SPI pins
foreign import ccall unsafe "bcm2835.h bcm2835_spi_begin" initSPI   :: IO ()
--resets the SPI pins
foreign import ccall unsafe "bcm2835.h bcm2835_spi_end"   stopSPI   :: IO ()

--Transfers one byte to and from the currently selected SPI slave
foreign import ccall unsafe "bcm2835_spi_transfer"        c_transferSPI :: CUChar -> IO CUChar
--Changes the chip select pins
foreign import ccall unsafe "bcm2835_spi_chipSelect"      c_chipSelectSPI :: CUChar -> IO ()


------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------ Exportable functions --------------------------------------------------------------------

------------------------------------------- Utility functions ----------------------------------------------------------------------
-- |Any IO computation that accesses the GPIO pins using this library should be wrapped with this function; ie @withGPIO $ do foo@.
-- It prepares the file descriptors to /dev/mem and makes sure everything is safely deallocated if an exception occurs. The behavior
-- when accessing the GPIO outside of this function is undefined.
withGPIO :: IO a -> IO a
withGPIO f = bracket    initBCM2835
                        (const stopBCM2835) --const because you don't care about the output of initBCM2835
                        (\a -> if a==0 then throwIO ioe else f) -- init returning 0 is not good
                            where ioe = IOError Nothing IllegalOperation "GPIO: " "Unable to start GPIO." Nothing Nothing

-- |Any IO computation that uses the I2C bus using this library should be wrapped with this function; ie @withI2C $ do foo@.
-- It prepares the relevant pins for use with the I2C protocol and makes sure everything is safely returned to normal if an exception
-- occurs. If you only use the GPIO pins for I2C, you can do @withGPIO . withI2C $ do foo@ and it will work as expected. WARNING: 
-- after this function returns, the I2C pins will be set to Input, so use 'setPinFunction' if you want to use them for output.
withI2C :: IO a -> IO a
withI2C f = bracket_    initI2C
                        stopI2C
                        f

--
actOnResult :: CUChar -> CString -> IO BS.ByteString
actOnResult rr buf = case rr of
    0x01 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received an unexpected NACK." Nothing Nothing
    0x02 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received Clock Stretch Timeout." Nothing Nothing 
    0x04 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Not all data was read." Nothing Nothing
    0x00 -> BS.packCString buf --convert C buffer to a bytestring


-- Mapping raspberry pi pin number to internal bmc2835 pin number, ugly solution, but meh. Also, the existence of mutiple versions
-- of the pin layout makes this the most elegant solution without resorting to state monads (which I don't want to do because that
-- would hamper the simplicity of having writePin and readPin be simple IO actions). As this function isn't exported anyway, the
-- user should never be troubled by all this.
getHwPin :: Pin -> CUChar
--P1 connector on V1 boards
getHwPin PinV1_03 = 0
getHwPin PinV1_05 = 1
getHwPin PinV1_07 = 4
getHwPin PinV1_08 = 14
getHwPin PinV1_10 = 15
getHwPin PinV1_11 = 17
getHwPin PinV1_12 = 18
getHwPin PinV1_13 = 21
getHwPin PinV1_15 = 22
getHwPin PinV1_16 = 23
getHwPin PinV1_18 = 24
getHwPin PinV1_19 = 10
getHwPin PinV1_21 = 9
getHwPin PinV1_22 = 25
getHwPin PinV1_23 = 11
getHwPin PinV1_24 = 8
getHwPin PinV1_26 = 7
--P1 connector on V2 boards
getHwPin Pin03 = 2
getHwPin Pin05 = 3
getHwPin Pin07 = 4
getHwPin Pin08 = 14
getHwPin Pin10 = 15
getHwPin Pin11 = 17
getHwPin Pin12 = 18
getHwPin Pin13 = 27
getHwPin Pin15 = 22
getHwPin Pin16 = 23
getHwPin Pin18 = 24
getHwPin Pin19 = 10
getHwPin Pin21 = 9
getHwPin Pin22 = 25
getHwPin Pin23 = 11
getHwPin Pin24 = 8
getHwPin Pin26 = 7
--for the P5 connector on V2 boards
getHwPin PinP5_03 = 28
getHwPin PinP5_04 = 29
getHwPin PinP5_05 = 30
getHwPin PinP5_06 = 31

------------------------------------------- GPIO functions -------------------------------------------------------------------------
-- |Sets the pin to either 'Input' or 'Output' mode.
setPinFunction :: Pin -> PinMode -> IO ()
setPinFunction pin mode = c_setPinFunction (getHwPin pin) (fromIntegral $ fromEnum mode)

-- |Sets the specified pin to either 'True' or 'False'.
writePin :: Pin -> LogicLevel -> IO () --wat gebeurt er als het geen output pin is?
writePin pin level = c_writePin (getHwPin pin) (fromIntegral $ fromEnum level)

-- |Returns the current state of the specified pin.
readPin :: Pin -> IO LogicLevel
readPin pin = (toEnum . fromIntegral) <$> c_readPin (getHwPin pin)

-------------------------------------------- I2C functions -------------------------------------------------------------------------
--not exported, only used internally
setI2cAddress :: Address -> IO ()
setI2cAddress a = c_setSlaveAddressI2C $ fromIntegral a

-- |Sets the clock divider for (and hence the speed of) the I2C bus.
setI2cClockDivider :: Word16 -> IO ()
setI2cClockDivider a = c_setClockDividerI2C $ fromIntegral a

-- |Sets the baud rate of the I2C bus.
setI2cBaudRate :: Word32 -> IO ()
setI2cBaudRate a = c_setBaudRateI2C $ fromIntegral a

-- |Writes the data in the 'ByteString' to the specified adress. Throws an IOException if an error occurs.
writeI2C :: Address -> BS.ByteString -> IO ()	--writes a bytestring to the specified address
writeI2C address by = BS.useAsCString by $ \bs -> do
    setI2cAddress address
    readresult <- c_writeI2C bs (fromIntegral $ BS.length by)
    case readresult of
        0x01 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received an unexpected NACK." Nothing Nothing
        0x02 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received Clock Stretch Timeout." Nothing Nothing 
        0x04 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Not all data was read." Nothing Nothing
        0x00 -> return ()

-- |Reads num bytes from the specified address. Throws an IOException if an error occurs.
readI2C :: Address -> Int -> IO BS.ByteString --reads num bytes from the specified address
readI2C address num = allocaBytes (num+1) $ \buf -> do --is the +1 necessary??
    setI2cAddress address
    readresult <- c_readI2C buf (fromIntegral num)
    actOnResult readresult buf

-- |Writes a 'ByteString' containing a register address to the specified address, then reads num bytes from
-- it, using the \"repeated start\" I2C method. Throws an IOException if an error occurs.
writeReadI2C :: Address -> BS.ByteString -> Int -> IO BS.ByteString
writeReadI2C address by num = BS.useAsCString by $ \bs -> do --marshall the register-containing bytestring
    allocaBytes (num+1) $ \buf -> do	--allocate a buffer for the response
        setI2cAddress address
        readresult <- c_writeReadI2C bs buf (fromIntegral num)
        actOnResult readresult buf
