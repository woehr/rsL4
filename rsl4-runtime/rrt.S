.text

.global rrt_start
rrt_start:
    /* Setup stack */
    ldr     sp, =_stack_top

    /* boot info in r0 */
    bl      init_boot_info

    /* enter Rust code */
    bl      main

