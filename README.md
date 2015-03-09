# rsL4

## An operating system built on seL4 in the Rust language

# Building

This project uses nix to perform builds. Nix is technically a package manager
but makes handling dependencies and performing clean builds very easy. Install
the nix package manager if you do not already have it.

Run `build.sh` and all dependencies required to build the project will be
downloaded and compiled. If successful, the result will be symlinked to
`result` in the base directory.

There are a few options that can be modified in `default.nix`, however this
is not recommened at this time.

# Libraries/modules

## rsl4-init

The initial kernel thread which sets up all other system services.

## rsl4-boot

This is the bootable component of the project. It is produced by combining the
kernel image and rsl4-init image into a single, bootable archive. To run with
u-boot, load the image into ram (address 0x82000000 on the am335x) and jump
to that address. See the seL4 documentation for more details.

## librsl4

Rust library that provides a low level interface to seL4 (similar to libsel4).

