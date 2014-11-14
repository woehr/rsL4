	.text
	.syntax unified
	.cpu	cortex-a8

	.globl	start
	.align	2
start:
# Setup our stack
	ldr sp, =STACK_START
# Allocate 8 bytes for llvm to use for the stack limit
	sub sp, #8
# Place the stack limit where llvm expects to find it
	ldr r0, =STACK_LIMIT
	str r0, [sp, 4]
# Set the 8 allocated bytes to be the TCB
	mcr p15, #0, sp, c13, c0, #3
# Branch to the Rust entry point
	bl main
# Should never get here
hang:
	b hang

