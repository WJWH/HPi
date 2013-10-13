HPi
===

Library to access the GPIO pins on a Raspberry Pi from Haskell.

===

HPi is a small library to access the GPIO pins on a Raspberry Pi from Haskell. It also includes some functions to use the I2C pins, see the haddock documentation for details. It is constructed as a FFI wrapper around the bcm2835 library, which is written in C. Because this library accesses the GPIO pins directly via a memory map, it should be faster than libraries which access the GPIO pins via the /sys/class/gpio interface.

===

In order to compile programs including this library, you will need to have the bcm2835 library installed. It can be found at http://airspayce.com/mikem/bcm2835/index.html. In addition, when compiling your program, include the file /path/to/bcm2835 library/src/bcm2835.o. For example, the example in the /test directory should be compiled as: ghc --make gpiotest.hs ~/bcm2835-1.25/src/bcm2835.o. Note that you cannot access the memory map without root privileges, programs should be run with sudo.

===

Questions, bug reports and feature requests are more than welcome!
