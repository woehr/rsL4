ENTRY(start);

STACK_SIZE  = 6K;

IMAGE_START = 0x402F0400;
STACK_START = 0x4030CE00;

STACK_END   = STACK_START - STACK_SIZE;
/* Be generous with extra stack at the end so we don't accidentally overflow the actual stack
 * when producing stack overflow debug output. */
STACK_LIMIT = STACK_END + 1024;

IMAGE_END   = STACK_END;
IMAGE_MAX   = IMAGE_END - IMAGE_START;

SECTIONS
{
  . = IMAGE_START;
  .text : {
    /* The entry point has to be where execution starts */
    _build/rsL4-boot-start.o (.text.start);

    *(.text);
  }

  .data : {
    *(.data);
    . = ALIGN(4);
    p_tcb = .;
    LONG(0);
    p_sl = .;
    LONG(0);
  }

  .bss : {
    .bss_start = .;
    *(.bss);
    .bss_end = .;
  }

  image_size = .bss_end - IMAGE_START;
}

ASSERT(image_size < IMAGE_MAX, "Image exceeds maximum allowable size.")

