; ================================================================
;	Example source with implicit target 'rom'
;	Copyright  (c)	Günter Woigk 1994 - 2015
;					mailto:kio@little-bat.de
; ================================================================


; If you don't declare a target and no code segment,
; then zasm silently assumes you are assembling a rom.
; In target 'rom' the fill byte for gaps is 0xff.
;
; the file size will be sized to fit, that is, it will not be padded to an unknown size.
; padding can be added with an explicit 'defs' instruction at the end of the source.
;
; this example will produce a 16k rom visible at address 0x0000.


		org 0

; reset:
RST0::	di
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

; maskable interrupt in mode 1:
		defs	0x38-$
RST7::	reti

; non maskable interrupt:
		defs   	0x66-$
NMI::	retn



; init:
; globals and statics initialization
; starts with copying the fixed data:

init:

; define some code here

		jr		$



; pad file to full eprom length, if desired:

		defs	0x4000 - $

