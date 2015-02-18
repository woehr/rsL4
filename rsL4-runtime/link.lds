ENTRY(rrt_start)

SECTIONS
{
    PROVIDE(__executable_start = 0x80000000);
    . = 0x80000000 + SIZEOF_HEADERS;

    .text : { *(.text*) }

    . = ALIGN(32);
    .rodata : { *(.rodata*) }

    . = ALIGN(32);
    .data : { *(.data) }

    . = ALIGN(32);
    .bss : { *(.bss .bss.*) }

    . = ALIGN(32);
    .rel.plt : { *(.rel.plt) }

    . = ALIGN(32);
    /* The LLVM generate stack check code expects the top of the stack to be stored
       at +4 bytes from the address stored in the user thread/process ID register */
    _tls = .;
    . += 8;

    _stack_top = .;
    . += 1024;
    _stack_bottom = .;
    . += 512; /* Slush factor for handling stack overflows */
    _stack_real_bottom = .;

}

