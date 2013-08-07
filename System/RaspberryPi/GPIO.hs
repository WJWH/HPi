module System.RaspberryPi.GPIO (
    initI2C,
    endI2C,
	setI2cAddress,
    setI2cClockDivider,
    setI2cBaudRate,
    writeI2C,
    readI2C,
    writeReadI2C
    ) where
    
--FFI wrapper over the I2C portions of the BCM2835 library by ..., also some utility functions to make reading and writing simpler

import Foreign
import Foreign.C
import Foreign.C.String
import qualified Data.ByteString as BS
import Data.Either


------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------Foreign imports--------------------------------------------------------------------------

------------------------------------------- I2C functions --------------------------------------------------------------------------
--inits the i2c pins
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_begin" initI2C :: IO ()
--resets the i2c pins
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_end"   endI2C  :: IO ()

--sets the slave address used
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_setSlaveAddress" c_setSlaveAddressI2C ::   CUChar -> IO ()
--sets the I2C bus clock divider (and thus, the speed)
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_setClockDivider" c_setClockDividerI2C ::   CUShort -> IO ()
--sets the I2C bus baud rate (100000 for the default 100khz)
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_set_baudrate"    c_setBaudRateI2C ::       CUInt -> IO ()

-- writes some bytes to the bus
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_write" c_writeI2C :: CString -> CUInt -> IO CUChar
--read some bytes from the bus
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_read" c_readI2c :: CString -> CUShort -> IO CUChar
--reads a certain register with the repeated start method
foreign import ccall unsafe "bcm2835.h bcm2835_i2c_read_register_rs" c_writeReadI2C :: CString -> CString -> CUShort -> IO CUChar

--handig: Data.ByteString.useAsCString :: ByteString -> (CString -> IO a) -> IO a 
-- const char * is in Haskell een CString

------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------Exportable functions---------------------------------------------------------------------

------------------------------------------- I2C functions --------------------------------------------------------------------------
setI2cAddress :: Address -> IO ()
setI2cAddress a = c_setSlaveAddressI2C $ fromIntegral a

setI2cClockDivider :: Word16 -> IO ()
setI2cClockDivider a = c_setClockDividerI2C $ fromIntegral a

setI2cBaudRate :: Word32 -> IO ()
setI2cBaudRate a = c_setBaudRateI2C $ fromIntegral a

writeI2C :: Address -> BS.ByteString -> IO ()	--writes a bytestring to the specified address
writeI2C address by = BS.useAsCString by $ \bs -> do
    setI2cAddress address
    c_writeI2C bs (fromIntegral $ BS.length by)

readI2C :: Address -> Int -> IO (Bool,BS.ByteString) --reads num bytes from the specified address
readI2C address num = allocaBytes (num+1) $ \buf -> do --is the +1 necessary??
    setI2cAddress address
    readresult <- c_readI2c buf num
	case readresult of
		0x01 -> return (False, "Received a NACK.")
		0x02 -> return (False, "Received Clock Stretch Timeout.")
		0x04 -> return (False, "Not all data was read.")
		0x00 -> do	bs <- BS.packCString buf --convert C buffer to a bytestring
					return (True, bs)

--writes a bytestring containing a register address to the specified  (i2c) address, then reads num bytes from it, using the repeated start i2c method
writeReadI2C :: Address -> BS.ByteString -> Int -> IO (Bool,BS.ByteString) 
writeReadI2C address by num = BS.useAsCString by $ \bs -> do --marshall the register-containing bytestring
	allocaBytes (num+1) $ \buf -> do	--allocate a buffer for the response
		setI2cAddress address
		readresult <- c_writeReadI2C bs buf num
		case readresult of
		0x01 -> return (Left "Received a NACK.")
		0x02 -> return (Left "Received Clock Stretch Timeout.")
		0x04 -> return (Left "Not all data was read.")
		0x00 -> do	bs <- BS.packCString buf --convert C buffer to a bytestring
					return (Right bs)