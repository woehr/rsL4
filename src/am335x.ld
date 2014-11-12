ENTRY(start);

STACK_SIZE  = 6K;

IMAGE_START = 0x402F0400;
STACK_START = 0x4030CE00;

STACK_LIMIT = STACK_START - STACK_SIZE + 16;

STACK_END   = STACK_START - STACK_SIZE;
IMAGE_END   = STACK_END;

IMAGE_MAX   = IMAGE_END - IMAGE_START;

SECTIONS
{
  image_size = .bss_end - IMAGE_START;

  . = IMAGE_START - 8;
  .text : {
    /* TI image header. See AM335x technical reference manuals section 26.1.9 */
    LONG(image_size);
    LONG(IMAGE_START);

    /* The entry point has to be at the header's destination (IMAGE_START) */
    obj/main.o (.text.start);

    *(.text);
  }

  .data : {
    *(.data);
  }

  .bss : {
    .bss_start = .;
    *(.bss);
    .bss_end = .;
  }

}

ASSERT(image_size < IMAGE_MAX, "Image exceeds maximum allowable size.")

