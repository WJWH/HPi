HPi
===

Library to access the GPIO pins on a Raspberry Pi from Haskell. Works with all Raspberry Pi versions up to and including v4.

===

HPi is a small library to access the GPIO pins on a Raspberry Pi from Haskell. It also includes some functions to use the I2C and SPI functionality of the Raspberry Pi, see the haddock documentation for details. It is constructed as a FFI wrapper around the `bcm2835` library, which is written in C. Because this library accesses the GPIO pins directly via a memory map, it should be faster than libraries which access the GPIO pins via the `/sys/class/gpio` interface.

===

In order to compile programs including this library, you will need to have the `bcm2835` library installed. It can be found at http://airspayce.com/mikem/`bcm2835`/index.html. When performing the cabal install for HPi you may need to pass the `--extra-lib-dirs` and `--extra-include-dirs` flags if you have installed `bcm2835` at a non-standard path. This library has been tested to work with `bcm2835` version 1.63. Note that you cannot access the memory map without root privileges, programs should be run with sudo.

===

Questions, bug reports and feature requests are more than welcome!
