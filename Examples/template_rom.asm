; ================================================================
;	Example source with target 'rom'
;	Copyright  (c)	Günter Woigk 1994 - 2015
;					mailto:kio@little-bat.de
; ================================================================


; same as 'bin', except that the default fill byte for 'defs' etc. is 0xff
; this example defines a 16k Eprom visible at address 0x0000 and an area
; for variables at 0x5B00 upward, which may be used for the ZX Spectrum


#target rom


#data VARIABLES, 0x5B00

; define some variables here



#code EPROM, 0, 0x4000


; reset vector
RST0::	di
		ld		sp,_ram_end
		jp		init
		defs	0x08-$

RST1::	reti
		defs	0x10-$

RST2::	reti
		defs	0x18-$

RST3::	reti
		defs	0x20-$

RST4::	reti
		defs	0x28-$

RST5::	reti
		defs	0x30-$

RST6::	reti
		defs	0x38-$

; maskable interrupt handler in interrupt mode 1:
RST7::	reti


; non maskable interrupt:
; e.g. call debugger and on exit resume.

		defs   	0x66-$
NMI::	retn



; init:
; globals and statics initialization
; starts with copying the fixed data:

init:

; define some code here

		jr	$


