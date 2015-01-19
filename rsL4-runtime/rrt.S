.text

.global rrt_start
rrt_start:
    /* Setup stack */
    ldr     sp, =_stack_top
    bl      main

