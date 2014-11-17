# rsL4

## An operating system built on seL4 in the Rust language

# Building

The build script is written using [Shake](https://github.com/ndmitchell/shake). GHC and Cabal will
need to be available on the command line. ARM versions of the gcc binutils is also required. See
`build.sh` for variables that can be modified.

To build, run `build.sh`.

# Libraries/modules

## rsL4-boot

This is a bootloader (currently only for the am335x platform, specifically the Beaglebone Black)
that loads the kernel and init images into external memory, sets-up the MMU, and enters the
kernel.

### rsL4-boot notes
The bootloader is booted from u-boot which does some setup for us, but we need to be wary
of the state u-boot has left us in. For now, we assume that no interrupts will fire
and that we don't need to relocate the vector table (u-boot will be running from external
memory and when we load the kernel/init images it's possible it will be overwritten.
Furthermore, we assume that external memory and the UART are setup.

