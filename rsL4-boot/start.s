	.text
	.syntax unified
	.cpu	cortex-a8
	.eabi_attribute	6, 10
	.eabi_attribute	7, 65
	.eabi_attribute	8, 1
	.eabi_attribute	9, 2
	.fpu	neon
	.eabi_attribute	15, 1
	.eabi_attribute	16, 1
	.eabi_attribute	17, 2
	.eabi_attribute	20, 1
	.eabi_attribute	21, 1
	.eabi_attribute	23, 3
	.eabi_attribute	24, 1
	.eabi_attribute	25, 1
	.eabi_attribute	14, 0
	.eabi_attribute	68, 1

# Bootloader entry point. This function sets up the stack and stack limit then branches to main
	.globl	start
	.align	2
start:
# Setup our stack
	ldr sp, =STACK_START
# Set the stack limit
	ldr r2, =STACK_LIMIT
	ldr r3, =p_sl
	str r2, [r3]
# Set the TCB to the 8 bytes set aside in the linker script
	ldr r2, =p_tcb
	mcr p15, #0, r2, c13, c0, #3
# Branch to the Rust entry point
	bl main
# Should never get here
hang:
	b hang

# Increases the stack limit to the maximum allowed. This function is used when __morestack needs
# stack for debug output.
	.globl	increase_stack
	.align	2
increase_stack:
	ldr r0, =STACK_END
	ldr r1, =p_sl
	str r0, [r1]
	mov pc, lr

