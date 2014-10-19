
; Example zasm source with implicit target 'rom':
; 
; If you don't declare a target and no code segment, 
; then zasm silently assumes you are assembling a rom.
; In target 'rom' the fill byte for gaps is 0xff.
;
; The 'org' instruction is required and must precede any real object code.
; the file size will be sized to fit, that is, it will not be padded to an unknown size.
; padding can be added with an explicit 'defs' or 'org' instruction at the end of the source.
;
; this example will produce a 16k rom visible at address 0x0000.


		org 0
	
; define some code here

		jr	$


; pad file to full eprom length, if desired:

		defs	0x4000 - $		; use one of both,
		org 	0x4000			; whichever you prefer.
