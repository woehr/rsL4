lib-dirs:=libs

all: kernel_elf libsel4 elfloader
	make -C tools/common -f Makefile.cpio_strip

-include .config
include tools/common/project.mk

