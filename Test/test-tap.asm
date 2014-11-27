

; Example tape file for ZX Spectrum
;
; fill byte is 0x00
; #segment has an additional argument: the sync byte for the block.
; The assembler calculates and appends checksum byte to each segment.
; Note: If a segment is appended without an explicite address, then the sync byte and the checksum byte
; of the preceding segment are not counted in calculating the start address of this segment.


#target tap


; sync bytes:
headerflag		= 0
dataflag		= 0xff


; some Basic tokens:
tCLEAR			= $FD		; token CLEAR
tLOAD			= $EF		; token LOAD
tCODE			= $AF		; token CODE
tPRINT			= $F5		; token PRINT
tRANDOMIZE		= $F9		; token RANDOMIZE
tUSR			= $C0		; token USR


ram_start		= $4000
pixels_start	= $4000		; ZXSP screen pixels
attr_start		= $5800		; ZXSP screen attributes
printer_buffer	= $5B00		; ZXSP printer buffer
ram_end			= $10000	; assuming 48k ram

code_start		= 24000
min_heap_size	= $1000		; for malloc




; ---------------------------------------------------
;		a Basic Loader:
; ---------------------------------------------------

#code PROG_HEADER,0,17,headerflag
		defb    0						; Indicates a Basic program
		defb    "mloader   "			; the block name, 10 bytes long
		defw    variables_end-0			; length of block = length of basic program plus variables
		defw    10		    			; line number for auto-start, 0x8000 if none
		defw    program_end-0			; length of the basic program without variables


#code PROG_DATA,0,*,dataflag

		; ZX Spectrum Basic tokens

; 10 CLEAR 23999
        defb    0,10                    ; line number
        defb    end10-($+1)             ; line length
        defb    0                       ; statement number
        defb    tCLEAR                  ; token CLEAR
        defm    "23999",$0e0000bf5d00   ; number 23999, ascii & internal format
end10:  defb    $0d                     ; line end marker

; 20 LOAD "" CODE 24000
        defb    0,20                    ; line number
        defb    end20-($+1)             ; line length
        defb    0                       ; statement number
        defb    tLOAD,'"','"',tCODE     ; token LOAD, 2 quotes, token CODE
        defm    "24000",$0e0000c05d00   ; number 24000, ascii & internal format
end20:  defb    $0d                     ; line end marker

; 30 RANDOMIZE USR 24000
        defb    0,30                    ; line number
        defb    end30-($+1)             ; line length
        defb    0                       ; statement number
        defb    tRANDOMIZE,tUSR         ; token RANDOMIZE, token USR
        defm    "24000",$0e0000c05d00   ; number 24000, ascii & internal format
end30:  defb    $0d                     ; line end marker

program_end:

		; ZX Spectrum Basic variables

variables_end:



; ---------------------------------------------------
;		a machine code block:
; ---------------------------------------------------

#code CODE_HEADER,0,17,headerflag
		defb    3						; Indicates binary data
		defb    "mcode     "	  		; the block name, 10 bytes long
		defw    code_end-code_start		; length of data block which follows
		defw    code_start				; default location for the data
		defw    0       				; unused


; the actual machine code
; this segment is split into several specially named segments
; because the c compiler needs it so:
;
#code CODE_DATA, code_start,*,dataflag


; ---------------------------------------------------
; Define ordering of code segments in ram:
; these segments produce code in the output file!
; since these segments are defined without start address and flag,
; they are appended to the previous tape segment by the assembler
; ---------------------------------------------------

#code 	_GSINIT				; init code: the compiler adds some code here and there as required
#code 	_HOME				; code that must not be put in a bank switched part of memory.
#code 	_CODE				; most code and const data go here
#code 	_CABS,*,0			; referenced but never (?) actually used by sdcc
#code 	_GSFINAL,*,0		; referenced but never (?) actually used by sdcc
#code 	_INITIALIZER		; initializer for initialized data in ram
							; if the code is started only once then you can overlay this with _INITIALIZED
							; instead of putting in the printer_buffer and skip the copy loop in _GSINIT


; ---------------------------------------------------
; Define variables in ram:
; note: 0x5B00 is the printer buffer
; note: the printer buffer is 256 bytes long
; note: system variables at 0x5C00 were initialized by Basic
; note: data segments do not produce actual code
; ---------------------------------------------------

#data 	_DATA, printer_buffer							; uninitialized data
#data 	_INITIALIZED, *, 0x100 - (_INITIALIZED-_DATA)	; data initialized from _INITIALIZER
#data	_DABS,*,0			; referenced but never (?) actually used by sdcc
#data	_RSEG,*,0			; referenced but never (?) actually used by kcc

#data 	_HEAP, code_end		; heap:
__sdcc_heap_start:	 		; --> sdcc _malloc.c
		ds	min_heap_size	; minimum required size
		ds	ram_end-$-1		; add all unused memory to the heap
__sdcc_heap_end: 			; --> sdcc _malloc.c
		ds 	1





; ================================================================
; globals and statics initialization:
; NOTE: evtl. this must go to the start of _GSINIT
;		in which case we cannot calculate _INITIALIZER_len in pass 1!
; ================================================================

#code _GSINIT

; set print channel to Screen:

		ld		a,2
		call	$1601


; print "Hello World"

		ld		hl,3$		;_hello_world
1$		ld		a,(hl)
		and		a
		jr		z,2$
		inc		hl
		rst		2
		jr		1$

3$		dm		13, "Hello World!", 13, 0
2$:


#code _GSINIT

; initialize initialized data:

		ld	bc,_INITIALIZER_len	; length of segment _INITIALIZER
		ld	de,_INITIALIZED		; start of segment _INITIALIZED
		ld	hl,_INITIALIZER		; start of segment _INITIALIZER
		ld	a,b
		or	c
		jr	z,$+4
		ldir

; more code may be added to segment _GSINIT by the C compiler
; the Z80 will run through it and then into the code of segment _HOME

#code _HOME

		jp	_main		; execute main() and return to BASIC



; ================================================================
; 	Input & Output:
; ================================================================

; extern char getchar(void);
;
; note:
;
; char getchar(void) { return inchar; }
; -->	ld	hl,#_inchar
;		ld	l,(hl)
;		ret

gc1:	halt					; wait for next interrupt -> next key press detect

_getchar::
		LD		HL,$5C3B        ; FLAGS
		RES		6,(HL)          ; signal string result.
;		BIT		7,(HL)          ; checking syntax ?
;		JR		Z,L2665         ; forward to S-INK$-EN if so

		CALL	$028E           ; routine KEY-SCAN key in E, shift in D.
;		LD		C,$00           ; the length of an empty string
;		JR		NZ,L2660        ; to S-IK$-STK to store empty string if no key returned.
		jr		nz,gc1			; no key available

		CALL	$031E           ; routine K-TEST get main code in A
;		JR		NC,L2660        ; to S-IK$-STK to stack null string if invalid
		jr		nc,gc1			; key is invalid

		DEC     D               ; D is expected to be FLAGS so set bit 3 $FF
								; 'L' Mode so no keywords.
		LD      E,A				; main key to A
								; C is MODE 0 'KLC' from above still.
		CALL    $0333			; routine K-DECODE
;		PUSH    AF				; save the code

        ld		l,a				; load key into return value
        ret


; extern void putchar(char);
;
; note:
;
; void putchar(char c) { outchar = c; }
; -->	push  ix
;		ld	  ix,#0
;		add	  ix,sp
;		ld	  a, 4 (ix)
;		ld	  (#_outchar),a
;		pop	  ix
;		ret

_putchar::
		ld	  hl,2
		add	  hl,sp
		ld	  a,(hl)			; a = char
		cp	  a,10				; '\n' ?
		jr	  nz,1$
		ld	  a,13				; replace 10 with 13
1$		rst	  2
		ret



; ================================================================
; 	The Payload:
; ================================================================

; SDCC does not generate a .globl statement for these labels:
; they must be declared before they are used,
; else they are not marked 'used' and "#include library" won't load them
;
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
	.globl	__sdcc_call_hl

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


; the test environment of sdcc is at a non-standard location.
; also IY must not be used else we can't call most rom routines
; --codeseg NAME		e.g. CODE for segment _CODE
; --std-sdcc89			default
; --std-c99				for bool
; --std-sdcc99			for bool
; --reserve-regs-iy		for ZX Spectrum system variables
; -fomit-frame-pointer
; --all-callee-saves
;
#cflags $CFLAGS --nostdinc -Iinclude --reserve-regs-iy


; include c files:
;
#include "main.c"
#include "../Examples/sdcc/device/lib/_days_per_month.c"
#include "../Examples/sdcc/device/lib/_asctime.c"

; resolved missing labels:
;
#include library "library"



; ================================================================
; calculate some last labels:
; ================================================================

#code _INITIALIZER

_INITIALIZER_len = $ - _INITIALIZER
code_end		 = $			; note: segment _INITIALIZER is the last code segment













