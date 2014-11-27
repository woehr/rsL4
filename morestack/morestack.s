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
	.file	"morestack.0.rs"
	.section	.text.__morestack,"ax",%progbits
	.globl	__morestack
	.align	2
	.type	__morestack,%function
__morestack:
	.fnstart
.Leh_func_begin0:
	.save	{r11, lr}
	push	{r11, lr}
	.setfp	r11, sp
	mov	r11, sp
	bl	_ZN11__morestack10__rust_abiE(PLT)
	pop	{r11, pc}
.Ltmp0:
	.size	__morestack, .Ltmp0-__morestack
.Leh_func_end0:
	.fnend

	.section	.text._ZN11__morestack10__rust_abiE,"ax",%progbits
	.align	2
	.type	_ZN11__morestack10__rust_abiE,%function
_ZN11__morestack10__rust_abiE:
	.fnstart
.Leh_func_begin1:
	b	.LBB1_1
.LBB1_1:
	b	.LBB1_1
.Ltmp1:
	.size	_ZN11__morestack10__rust_abiE, .Ltmp1-_ZN11__morestack10__rust_abiE
.Leh_func_end1:
	.fnend


