
	org	0

	db	10
	db	$10
	db	%10
	db	10h
	db	10b
	db	0x10
	db	'A'
	db	-123
	db	+123
	db	0
	db	2
	db	0b
	db	1b
	db	8h
	db	0b1010
	db	0b10

!	db	2b
!	db	0x
!	db	20b
!	db	0xAGGA
!	db	123A4
!	db	$AZZE
!	db	%012
!	db	%210

!#endif

!    ADC  IX,BC

#include "test-opcodes.asm"

	ld	hl,4
	add	hl,sp
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	