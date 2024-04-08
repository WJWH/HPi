{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

-- |Library for controlling the GPIO pins on a Raspberry Pi (or any system using the Broadcom 2835 SOC). It is constructed
-- as a FFI wrapper over the BCM2835 library by Mike McCauley.
module System.RaspberryPi.GPIO (
    -- *Data types
    Pin(..),
    PinMode(..),
    LogicLevel,
    Address,
    SPIBitOrder(..),
    SPIPin(..),
    CPOL,
    CPHA,
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
    writeReadRSI2C,
    -- *SPI specific functions
    withAUXSPI,
    withSPI,
    chipSelectSPI,
    setBitOrderSPI,
    setChipSelectPolaritySPI,
    setClockDividerAUXSPI,
    setClockDividerSPI,
    setDataModeSPI,
    transferAUXSPI,
    transferSPI,
    transferManySPI,
    -- *PWM specific functions
    -- |Allows control of 2 independent PWM channels. A limited subset of GPIO
    -- pins can be connected to one of these 2 channels, allowing PWM control
    -- of GPIO pins. You have to set the desired pin into a particular Alt Fun
    -- to PWM output.
    setClockPWM,
    setModePWM,
    setRangePWM,
    setDataPWM
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
-- |This describes the pins on the Raspberry Pi boards. Since the BCM2835 SOC internally uses different numbers (and these numbers 
-- differ between versions, the library internally translates this pin number to the correct number.
data Pin =  -- |Pins for the P1 connector of the V2 revision of the Raspberry Pi
            Pin03|Pin05|Pin07|Pin08|Pin10|Pin11|Pin12|Pin13|Pin15|Pin16|Pin18|Pin19|Pin21|Pin22|Pin23|Pin24|Pin26|Pin32|Pin33|Pin35|Pin36|
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

-- |Specifies the SPI data bit ordering.
data SPIBitOrder = LSBFirst | MSBFirst

-- |This describes which Chip Select pins are asserted (used in SPI communications).
data SPIPin = CS0 | CS1 | CS2 | CSNONE deriving (Eq, Show, Enum)

-- |Clock polarity (CPOL) for SPI transmissions.
type CPOL = Bool

-- |Clock phase (CPHA) for SPI transmissions.
type CPHA = Bool

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
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_write_read_rs" c_writeReadRSI2C :: CString -> CUInt -> CString -> CUInt -> IO CUChar

------------------------------------------- SPI functions --------------------------------------------------------------------------
--inits the SPI pins
foreign import ccall unsafe "bcm2835.h bcm2835_spi_begin" initSPI   :: IO ()
--resets the SPI pins
foreign import ccall unsafe "bcm2835.h bcm2835_spi_end"   stopSPI   :: IO ()

--Transfers one byte to and from the currently selected SPI slave
foreign import ccall unsafe "bcm2835.h bcm2835_spi_transfer"        c_transferSPI :: CUChar -> IO CUChar
--Transfers multiple bytes to and from the currently selected SPI slave
foreign import ccall unsafe "bcm2835.h bcm2835_spi_transfern"      c_transferManySPI :: CString -> CUInt -> IO ()
--Changes the chip select pins
foreign import ccall unsafe "bcm2835.h bcm2835_spi_chipSelect"      c_chipSelectSPI :: CUChar -> IO ()

--Set the bit order to be used for transmit and receive.
foreign import ccall unsafe "bcm2835.h bcm2835_spi_setBitOrder"           c_setBitOrder :: CUChar -> IO ()
--Sets whether SPI Chip Select pulls pins high or low.
foreign import ccall unsafe "bcm2835.h bcm2835_spi_setChipSelectPolarity" c_setChipSelectPolarity :: CUChar -> CUChar -> IO ()
--Sets the SPI clock divider and therefore the SPI clock speed.
foreign import ccall unsafe "bcm2835.h bcm2835_spi_setClockDivider"       c_setClockDividerSPI :: CUShort -> IO ()
--Sets the data mode used (phase/polarity)
foreign import ccall unsafe "bcm2835.h bcm2835_spi_setDataMode"     c_setDataModeSPI :: CUChar -> IO ()

----------------------------------------- AUX SPI functions ------------------------------------------------------------------------
--inits the AUX SPI pins
foreign import ccall unsafe "bcm2835.h bcm2835_aux_spi_begin" initAUXSPI   :: IO Int
--resets the AUX SPI pins
foreign import ccall unsafe "bcm2835.h bcm2835_aux_spi_end"   stopAUXSPI   :: IO ()

--Transfers one byte to and from the currently selected SPI slave
foreign import ccall unsafe "bcm2835.h bcm2835_aux_spi_transfer"       c_transferAUXSPI :: CUChar -> IO CUChar

--Sets the SPI clock divider and therefore the SPI clock speed.
foreign import ccall unsafe "bcm2835.h bcm2835_aux_spi_setClockDivider"     c_setClockDividerAUXSPI :: CUShort -> IO ()

------------------------------------------- PWM functions --------------------------------------------------------------------------
-- sets the PWM clock
foreign import ccall unsafe "bcm2835.h bcm2835_pwm_set_clock" c_setClockPWM :: CUInt -> IO ()

-- sets the PWM pulse ratio to emit to DATA/RANGE
foreign import ccall unsafe "bcm2835.h bcm2835_pwm_set_data" c_setDataPWM :: CUChar -> CUInt -> IO ()

-- sets the mode of the given PWM channel
foreign import ccall unsafe "bcm2835.h bcm2835_pwm_set_mode" c_setModePWM :: CUChar -> CUChar -> CUChar -> IO ()

-- sets the maximum range of the PWM output
foreign import ccall unsafe "bcm2835.h bcm2835_pwm_set_range" c_setRangePWM :: CUChar -> CUInt -> IO ()

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

-- |Any IO computation that uses the SPI functionality using this library should be wrapped with this function; ie @withSPI $ do foo@.
-- It prepares the relevant pins for use with the SPI protocol and makes sure everything is safely returned to normal if an exception
-- occurs. If you only use the GPIO pins for SPI, you can do @withGPIO . withSPI $ do foo@ and it will work as expected. WARNING: 
-- after this function returns, the SPI pins will be set to Input, so use 'setPinFunction' if you want to use them for output.
withSPI :: IO a -> IO a
withSPI f = bracket_    initSPI
                        stopSPI
                        f
                        
-- Possible error results for I2C functions. (not exported)
actOnResult :: CUChar -> CStringLen -> IO BS.ByteString
actOnResult rr buf = case rr of
    0x01 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received an unexpected NACK." Nothing Nothing
    0x02 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received Clock Stretch Timeout." Nothing Nothing 
    0x04 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Not all data was read." Nothing Nothing
    0x00 -> BS.packCStringLen buf --convert C buffer to a bytestring


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
getHwPin Pin32 = 12
getHwPin Pin33 = 13
getHwPin Pin35 = 19
getHwPin Pin36 = 16
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

-- |Writes the data in the 'ByteString' to the specified I2C 'Address'. Throws an IOException if an error occurs.
writeI2C :: Address -> BS.ByteString -> IO () --writes a bytestring to the specified address
writeI2C address by = BS.useAsCStringLen by $ \(bs,len) -> do
    setI2cAddress address
    readresult <- c_writeI2C bs (fromIntegral len)
    case readresult of
        0x01 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received an unexpected NACK." Nothing Nothing
        0x02 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Received Clock Stretch Timeout." Nothing Nothing 
        0x04 -> throwIO $ IOError Nothing IllegalOperation "I2C: " "Not all data was read." Nothing Nothing
        0x00 -> return ()

-- |Reads num bytes from the specified 'Address'. Throws an IOException if an error occurs.
readI2C :: Address -> Int -> IO BS.ByteString --reads num bytes from the specified address
readI2C address num = allocaBytes (num+1) $ \buf -> do --is the +1 necessary??
    setI2cAddress address
    readresult <- c_readI2C buf (fromIntegral num)
    actOnResult readresult (buf, num)

-- |Writes the data in the 'ByteString' to the specified 'Address', then issues a "repeated start" (with no prior stop) and then 
-- reads num bytes from the same 'Address'. Necessary for devices that require such behavior, such as the MLX90620.
writeReadRSI2C :: Address -> BS.ByteString -> Int -> IO BS.ByteString
writeReadRSI2C address by num = BS.useAsCStringLen by $ \(bs,len) -> do --marshall the register-containing bytestring
    allocaBytes num $ \buf -> do --allocate a buffer for the response
        setI2cAddress address
        readresult <- c_writeReadRSI2C bs (fromIntegral len) buf (fromIntegral num)
        actOnResult readresult (buf, num)
        
-------------------------------------------- SPI functions -------------------------------------------------------------------------
-- |Sets the chip select pin(s). When a transfer is made with 'transferSPI' or 'transferManySPI', the selected pin(s) will be 
-- asserted during the transfer. 
chipSelectSPI :: SPIPin -> IO ()
chipSelectSPI pin = c_chipSelectSPI (fromIntegral . fromEnum $ pin)

-- |Sets the SPI clock divider and therefore the SPI clock speed.
setClockDividerSPI :: Word16 -> IO ()
setClockDividerSPI a = c_setClockDividerSPI $ fromIntegral a

-- |Set the bit order to be used for transmit and receive. The bcm2835 SPI0 only supports MSBFirst,
-- so if you select LSBFirst, the bytes will be reversed in software.
-- The library defaults to MSBFirst.
setBitOrderSPI :: SPIBitOrder -> IO ()
setBitOrderSPI LSBFirst = c_setBitOrder 0
setBitOrderSPI MSBFirst = c_setBitOrder 1

-- |Sets the chip select pin polarity for a given pin(s). When a transfer is made with 'transferSPI' or 'transferManySPI', the 
-- currently selected chip select pin(s) will be asserted to the LogicLevel supplied. When transfers are not happening, the chip 
-- select pin(s) return to the complement (inactive) value. 
setChipSelectPolaritySPI :: SPIPin -> LogicLevel -> IO ()
setChipSelectPolaritySPI pin level = c_setChipSelectPolarity (fromIntegral . fromEnum $ pin) (fromIntegral . fromEnum $ level)

-- |Sets the SPI clock polarity and phase (ie, CPOL and CPHA)
setDataModeSPI :: (CPOL,CPHA) -> IO ()
setDataModeSPI (False,False) = c_setDataModeSPI 0
setDataModeSPI (False,True)  = c_setDataModeSPI 1
setDataModeSPI (True,False)  = c_setDataModeSPI 2
setDataModeSPI (True,True)   = c_setDataModeSPI 3

-- |Transfers one byte to and from the currently selected SPI slave. Asserts the currently selected CS pins (as previously set by 
-- 'chipSelectSPI') during the transfer. Clocks the 8 bit value out on MOSI, and simultaneously clocks in data from MISO. Returns the 
-- read data byte from the slave.
transferSPI :: Word8 -> IO Word8
transferSPI input = fromIntegral <$> c_transferSPI (fromIntegral input)

-- |Transfers any number of bytes to and from the currently selected SPI slave, one byte at a time. Asserts the currently selected 
-- CS pins (as previously set by 'chipSelectSPI') during the transfer. Clocks 8 bit bytes out on MOSI, and simultaneously clocks in 
-- data from MISO.
transferManySPI :: [Word8] -> IO [Word8]
transferManySPI input = BS.useAsCStringLen (BS.pack input) $ \(buf,len) -> do --convert input list to bytestring and from there to CString
    --returns the read bytes in buf. Uses CStringLen because the responses might have zero bytes and this will influence the result if a
    --normal CString is used
    c_transferManySPI buf (fromIntegral len) --
    (BS.packCStringLen (buf,len)) >>= return . BS.unpack -- translate back from a buffer to a bytestring to a [Word8]
        
------------------------------------------ AUX SPI functions -----------------------------------------------------------------------

-- |Any IO computation that uses the AUX SPI functionality using this library should be wrapped with this function; ie @withAUXSPI $ do foo@.
-- It prepares the relevant pins for use with the SPI protocol and makes sure everything is safely returned to normal if an exception
-- occurs. If you only use the GPIO pins for SPI, you can do @withGPIO . withAUXSPI $ do foo@ and it will work as expected. WARNING: 
-- after this function returns, the SPI pins will be set to Input, so use 'setPinFunction' if you want to use them for output.
withAUXSPI :: IO a -> IO a
withAUXSPI f = bracket initAUXSPI
                       (const stopAUXSPI)
                       (\r -> if r==0 then throwIO ioe else f)
                            where ioe = IOError Nothing IllegalOperation "AUXAPI: " "Unable to start AUXAPI." Nothing Nothing

-- |Sets the AUX SPI clock divider and therefore the SPI clock speed.
setClockDividerAUXSPI :: Word16 -> IO ()
setClockDividerAUXSPI a = c_setClockDividerAUXSPI $ fromIntegral a

-- |Transfers one byte to and from the SPI slave. Asserts the CS2 pin during the transfer. Clocks the 8 bit value out
-- on MOSI, and simultaneously clocks in data from MISO. Returns the read data byte from the slave.
transferAUXSPI :: Word8 -> IO Word8
transferAUXSPI input = fromIntegral <$> c_transferAUXSPI (fromIntegral input)

-------------------------------------------- PWM functions -------------------------------------------------------------------------

-- |Sets the PWM clock divisor, to control the basic PWM pulse widths. 
setClockPWM :: Word32 -> IO ()
setClockPWM a = c_setClockPWM $ fromIntegral a

-- |Sets the mode of the given PWM channel, allowing you to control the PWM mode and enable/disable that channel 
setModePWM :: Word8 -> Word8 -> Word8 -> IO ()
setModePWM a b c = c_setModePWM (fromIntegral a) (fromIntegral b) (fromIntegral c)

-- |Sets the maximum range of the PWM output. The data value can vary between 0 and this range to control PWM output 
setRangePWM :: Word8 -> Word32 -> IO ()
setRangePWM a b = c_setRangePWM (fromIntegral a) (fromIntegral b)

-- |Sets the PWM pulse ratio to emit to DATA/RANGE, where RANGE is set by 'setRangePWM'.
setDataPWM :: Word8 -> Word32 -> IO ()
setDataPWM a b = c_setDataPWM (fromIntegral a) (fromIntegral b)

