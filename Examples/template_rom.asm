
; Example zasm source with target 'rom':
; 
; same as 'bin', except that the default fill byte for 'defs' etc. is 0xff
; this example defines a 16k Eprom visible at address 0x0000 and an area 
; for variables at 0x5B00 upward, which may be used for the ZX Spectrum


#target rom


#data VARIABLES, 0x5B00

; define some variables here



#code EPROM, 0, 0x4000

; define some code here

		jr	$
