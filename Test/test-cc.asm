;
;			Example C project for use with zasm
; 			(c) 2014 - 2014	kio@little-bat.de 
;


; declare target:
; (required because we use segments)
;
#target rom


; define rom and ram size and address:
;
_rom_start::		equ	0
_rom_end::			equ	0x4000
_ram_start::		equ	0x4000
_ram_end::			equ	0x10000
_min_heap_size::	equ	0x1000	; for malloc




; ________________________________________________________________
; Define ordering of code segments in rom:
; these will produce code in the output file!
;
#code 	_HEADER, _rom_start		; RST vectors et.al.
#code 	_HOME					; code that must not be put in a bank switched part of memory.
#code 	_CODE					; most code and const data go here
#code 	_GSINIT					; init code: the compiler adds some code here and there as required
								; note: the final ret from _GSINIT is at the end of this file.
#code 	_INITIALIZER			; initializer for initialized data in ram
		org 0					; note: -> make segment length evaluatable in pass 1
								; note: _INITIALIZER_len is calculated at end of this file
#code	_ROM_PADDING
		defs	_rom_end-$		; pad rom file up to rom end


; ________________________________________________________________
; Define ordering of data segments in ram:
; define addresses: no actual code stored in the output file!
;
#data 	_DATA, _ram_start		; uninitialized data
#data 	_INITIALIZED			; initialized with data from code segment _INITIALIZER
		
#data 	_HEAP					; heap
__sdcc_heap_start:: 			; --> sdcc malloc.c
		ds	_min_heap_size		; minimum required size for malloc/free
		ds	_ram_end-$-1		; add all unused memory to the heap
__sdcc_heap_end:: 				; --> sdcc malloc.c
		ds 	1


; ________________________________________________________________
; Declare segments which are referenced by sdcc 
; but never (?) used:
;
#data	_DABS,*,0				; used by sdcc: .area _DABS (ABS): absolute external ram data?
#code 	_CABS,*,0				; used by sdcc: .area _CABS (ABS): ?
#code 	_GSFINAL,*,0			; used by sdcc: .area _GSFINAL: ?




; ================================================================
; 	_HEADER segment: 
; 	starts at 0x0000
; ================================================================

;	reset vector
;	RST vectors
;	INT vector (IM 1)
;	NMI vector
;
#code _HEADER

; reset vector
RST0::	di
		ld		sp,0x0000	; Set stack pointer directly above top of memory.
		jp		_GSINIT		; Initialize global variables, call main() and exit

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




; ________________________________________________________________
; maskable interrupt handler in IM 1:
;
		defs	0x38-$
RST7::	reti



; ________________________________________________________________
; non-maskable interrupt handler:
; must return with RETN
;
		defs   	0x66-$		
NMI::	rst		0
;		retn



; ================================================================
; 	The Payload:
; ================================================================

#include "main.c"


; ________________________________________________________________
; resolved missing labels from libraries:
;
; TODO



; ================================================================
; 	Final Definitions & Code
;	must be after all other code
; ================================================================


; ________________________________________________________________
; calculate length of segment _INITIALIZER
; this requires an 'org 0' instruction at the start of this segment
;
#code	_INITIALIZER
_INITIALIZER_len:: equ $-0


; ________________________________________________________________
; globals and statics initialization:
; NOTE: evtl. this must go to the start of _GSINIT
;		in which case we cannot calculate _INITIALIZER_len in pass 1!
;
#code _GSINIT
#if _INITIALIZER_len!=0			; must be  evaluatable in pass 1
		ld	bc,_INITIALIZER_len	; length of segment _INITIALIZER
		ld	de,_INITIALIZED		; start of segment _INITIALIZED
		ld	hl,_INITIALIZER		; start of segment _INITIALIZER
;		ld	a,b
;		or	c
;		jr	z,$+4
		ldir		
#endif


; ________________________________________________________________
; start the system
; after all initialization in _GSINIT is done
;
#code 	_GSINIT
		call	_main		; execute main()
_exit::	di					; system shut down
1$: 	halt				; may resume after NMI
		rst		0			; then reboot





#end










