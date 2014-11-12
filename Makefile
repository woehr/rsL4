TOOLCHAIN = arm-none-eabi-

# See llc for available processors and targets
PROCESSOR = cortex-a8
TRIPLE = arm-unknown-linux-gnueabi

ARM_OPTS = --target $(TRIPLE) -C target-cpu=$(PROCESSOR)

RUSTC_BRANCH = master

all: bin/mlo

bin/mlo: bin/rsL4-boot
	$(TOOLCHAIN)objcopy -O binary ./bin/rsL4-boot ./bin/mlo

bin/rsL4-boot: bin-dir lib/libcore.rlib obj/main.o src/am335x.ld
	$(TOOLCHAIN)ld -T ./src/am335x.ld -o bin/rsL4-boot lib/libcore.rlib obj/main.o

obj/main.o: obj-dir src/main.rs
	rustc $(ARM_OPTS) -L ./lib/ --out-dir ./obj/ --emit asm,obj,ir ./src/main.rs

lib/libcore.rlib: lib-dir rust-src
	rustc $(ARM_OPTS) --out-dir lib ./rust-src/src/libcore/lib.rs

lib-dir:
	mkdir -p ./lib

obj-dir:
	mkdir -p ./obj

bin-dir:
	mkdir -p ./bin

update: rust-src
	cd rust-src && git fetch origin && git checkout --force $(RUSTC_BRANCH)

rust-src:
	if [ ! -d "rust-src" ]; then git clone https://github.com/rust-lang/rust rust-src; fi

clean:
	rm -rf -src lib obj bin

# Clean target that will wipe out the rust source. Not recommended since it takes a while to download
clean-all: clean
	rm -rf rust-src

