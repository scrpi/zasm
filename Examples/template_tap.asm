

; Example file for target 'tap'
; Tape file for ZX Spectrum and Jupiter ACE.
;
; fill byte is 0x00
; #segment has an additional argument: the sync byte for the block.
; The assembler calculates and appends checksum byte to each segment.
; Note: If a segment is appended without an explicite address, then the sync byte and the checksum byte 
; of the preceding segment are not counted in calculating the start address of this segment.


#target tap


; sync bytes:
headerflag:     equ 0
dataflag:       equ 0xff


; some Basic tokens:
tCLEAR  equ     $FD             ; token CLEAR
tLOAD   equ     $EF             ; token LOAD
tCODE   equ     $AF             ; token CODE
tPRINT  equ     $F5             ; token PRINT
tUSR    equ     $C0             ; token USR


pixels_start	equ	0x4000		; ZXSP screen pixels
attr_start		equ	0x5800		; ZXSP screen attributes
printer_buffer	equ	0x5B00		; ZXSP printer buffer
code_start		equ	24000



; ---------------------------------------------------
;		ram-based, non-initialized variables
;		(note: 0x5B00 is the printer buffer)
;		(note: system variables at 0x5C00 were initialized by Basic)
; ---------------------------------------------------

#data VARIABLES, printer_buffer, 0x100

; define some variables here



; ---------------------------------------------------
;		a Basic Loader:
; ---------------------------------------------------

#code PROG_HEADER,0,17,headerflag
		defb    0						; Indicates a Basic program
		defb    "mloader",0,0,0			; the block name, 10 bytes long
		defw    variables_end-0			; length of block = length of basic program plus variables
		defw    0x8000	    			; line number for auto-start, 0x8000 if none
		defw    program_end-0			; length of the basic program without variables
		

#code PROG_DATA,0,*,dataflag

		; ZX Spectrum Basic tokens 
		
; 10 CLEAR 24000
        defb    0,10                    ; line number
        defb    end10-($+1)             ; line length
        defb    0                       ; statement number
        defb    tCLEAR                  ; token CLEAR
        defm    "24000",$0e0000c05d00   ; number 24000, ascii & internal format
end10:  defb    $0d                     ; line end marker

; 20 LOAD "" CODE 24000
        defb    0,20                    ; line number
        defb    end20-($+1)             ; line length
        defb    0                       ; statement number
        defb    tLOAD,'"','"',tCODE     ; token LOAD, 2 quotes, token CODE
        defm    "24000",$0e0000c05d00   ; number 24000, ascii & internal format
end20:  defb    $0d                     ; line end marker

; 30 PRINT USR 24000
        defb    0,30                    ; line number
        defb    end30-($+1)             ; line length
        defb    0                       ; statement number
        defb    tPRINT,tUSR             ; token PRINT, token USR
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
		defb    "mcode",0,0,0,0,0  		; the block name, 10 bytes long
		defw    code_end-code_start		; length of data block which follows
		defw    code_start				; default location for the data
		defw    0       				; unused		


#code CODE_DATA, code_start,*,dataflag

; Z80 assembler code and data

		ret
		
code_end:












