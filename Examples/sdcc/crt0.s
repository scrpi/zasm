; ------------------------------------------------------------------------
;	crt0.s - Generic boot loader for a Z80 system
; ------------------------------------------------------------------------


_rom_start::		equ	0
_rom_end::			equ	0x4000
_ram_start::		equ	0x4000
_ram_end::			equ	0x10000
_min_heap_size::	equ	0x1000


; Ordering of segments:
; segments which i have not yet seen being used are set to size 0 to trigger an error if they are used
;
; Ordering of code segments in the resulting file:
;
#code 	_HEADER,_rom_start
#code 	_HOME					; Code that should never be put in a bank switched part of memory.
#code 	_CODE					; most code and const data go here
#code 	_CABS,*,0				; ?
#code 	_INITIALIZER			; initializer for initialized data in ram
#code 	_GSINIT					; init code: the compiler adds some code here and there as required
#code 	_GSFINAL,*,1			; the final ret from gsinit:: in _GSINIT
		ret
#code	_GSEXIT,*,0
#code	_GSEXITFINAL,*,0
#code	_ROM_PADDING
		defs	_rom_end-$$		; pad rom file up to rom end

; Ordering of data segments:
; not stored in the output file!
;
#data 	_DATA,_ram_start		; data in ram: uninitialized data
#data 	_INITIALIZED			; data in ram: initialized with data from segment _INITIALIZER
#data 	_BSEG,*,0				; data in ram: TODO: ? absolute segment within BIT space (Ax51) ?
#data 	_BSS,*,0				; data in ram: uninitialized
#data	_DABS,*,0				; absolute external ram data
#data 	_HEAP,*,_min_heap_size	; data in ram: heap: minimum required size for malloc/free
#data	_HEAP_EXTRA				; any additional unused memory adds to the heap
		defs	_ram_end-$-1
#data	_HEAP_END,*,1
		defs	1				; malloc lib provided by sdcc needs it this way




; -------------------------------------------------------------
; 	_HEADER segment: 
; 	starts at 0x0000
; -------------------------------------------------------------
;
;	reset vector
;	RST vectors
;	NMI vector
;	INT vector (IM 1)
;
#code _HEADER

; reset vector
RST0::	jp		init		
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

; maskable interrupt handler in IM 1:
RST7::	reti


; NMI: muss mit retn abgeschlossen werden
		defs   	0x66-$		
NMI::	push 	af
		push 	bc
		push 	de
		push 	hl
		push 	iy
		push 	ix
		call 	_cv_vint	; Aufruf Handler im C Modul main.c
		pop 	ix
		pop 	iy
		pop 	hl
		pop 	de
		pop 	bc
		pop 	af
		retn
	
#code _CODE
__clock::					; TODO: ?
		ret

init:	di
		ld		sp,0x0000	; Set stack pointer directly above top of memory.
		call    gsinit		; Initialise global variables
		call	_main		; execute main()
		jp		_exit		; system shut down

_exit::
		di
1$: 	halt
		jr		1$


#code _GSINIT
;
; globals and statics initialization:
;
gsinit::
#if l__INITIALIZER!=0
		ld	bc,l__INITIALIZER	; length of segment _INITIALIZER
		ld	de,s__INITIALIZED	; start of segment _INITIALIZED
		ld	hl,s__INITIALIZER	; start of segment _INITIALIZER
		ldir
#endif












