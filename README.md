HPi
===

Library to access the GPIO pins on a Raspberry Pi from Haskell.

===

HPi is a small library to access the GPIO pins on a Raspberry Pi from Haskell. It also includes some functions to use the I2C pins. See the haddock documentation for details.

===

In order to compile programs including this library, you will need to have the bcm2835 library installed. It can be found at http://airspayce.com/mikem/bcm2835/index.html. In addition, when compiling your program, include the file /path/to/bcm2835 library/src/bcm2835.o. For example, the example in the /test directory should be compiled as: ghc --make gpiotest.hs ~/bcm2835-1.25/src/bcm2835.o. 

===

Questions, bug reports and feature requests are more than welcome!
