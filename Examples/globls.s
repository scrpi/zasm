; ================================================================
;	.globl declarations for global labels in library files
;	Copyright  (c)	Günter Woigk 1994 - 2014
;					mailto:kio@little-bat.de
; ================================================================

; SDCC does not generate a .globl statement for internally used global labels:
;
; 	Labels from libraries must be declared before they are referenced,
; 	else they are not marked as 'used' when they are referenced
; 	and then "#include library" won't load them.
; 	They'll remain undefined and assembly will fail.
;
;	Simply included this file in the asm source before the first c file is included.

	.globl	__sdcc_call_hl
	.globl	_memcpy			; sdcc may or may not inline appropriate code --> include/string.h
	.globl	_strcpy			; ""
	.globl	_strchr			; ""
	.globl	_strncpy		; ""
	.globl	_memset			; ""

	.globl	___fsmul
	.globl	___fsdiv
	.globl	___fslt
	.globl	___fsgt
	.globl	___fsadd
	.globl	___fssub
	.globl	___fs2ulong
	.globl	___ulong2fs

	.globl	__rrulong
	.globl	__rrslong
	.globl	__rlulong
	.globl	__rlslong

	.globl	__mullonglong
	.globl	__mullong
	.globl	__mulint
	.globl	__muluchar
	.globl	__mulschar
	.globl	__mulsuchar
	.globl	__muluschar

	.globl	__modulong
	.globl	__modslong
	.globl	__modsint
	.globl	__moduint
	.globl	__modschar
	.globl	__moduchar
	.globl	__moduschar
	.globl	__modsuchar

	.globl	__divulong
	.globl	__divslong
	.globl	__divsint
	.globl	__divuint
	.globl	__div16
	.globl	__divu16
	.globl	__divuchar
	.globl	__divschar
	.globl	__divuschar
	.globl	__div8
	.globl	__divu8

	.globl	__get_remainder
	.globl	__div_signexte
